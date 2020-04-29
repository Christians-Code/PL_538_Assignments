use rss::Channel;
use std::collections::HashSet;
use std::fs::File;
use std::io::BufReader;
use std::result::Result;

use std::sync::{Arc, Mutex};
use url::Url;

use crate::common::*;
use crate::threadpool::*;

/// Thread pool sizes.
const SIZE_FEEDS_POOL: usize = 3;
const SIZE_SITES_POOL: usize = 20;

/// Same as the single/multi threaded version, but using a thread pool. Set up two thread pools:
/// one for handling feeds, and one for handling articles. Use the sizes above. Push closures
/// executing `process_feed` into the thread pool.
pub fn process_feed_file(file_name: &str, index: Arc<Mutex<ArticleIndex>>) -> RssIndexResult<()> {
    let file = File::open(file_name)?;
    println!("Processing feed file: {}", file_name);

    let channel = Channel::read_from(BufReader::new(file))?;

    let urls = Arc::new(Mutex::new(HashSet::new()));

    let feed_pool = Mutex::new(ThreadPool::new(SIZE_FEEDS_POOL));
    let sites_pool = Arc::new(Mutex::new(ThreadPool::new(SIZE_SITES_POOL)));

    for feed in channel.into_items() {
        let urls = Arc::clone(&urls);
        let index = Arc::clone(&index);
        let sites_pool = Arc::clone(&sites_pool);

        feed_pool.lock().unwrap().execute(move || {
            let url_option = feed.link();
            match url_option {
                Some(url) => {
                    let title_option = feed.title();
                    match title_option {
                        Some(title) => {
                            if urls.lock().unwrap().contains(url) {
                                println!("Skipping already seen feed: {} [{}]", title, url);
                            }
                            urls.lock().unwrap().insert(url.to_string());

                            println!("Processing feed: {} [{}]", title, url);

                            process_feed(url, index, urls, sites_pool).unwrap_or_default();

                            return ();
                        }
                        None => return (),
                    }
                }
                None => return (),
            };
        });
    }

    Result::Ok(())
}

/// Same as the single/multi threaded version, but using a thread pool. Push closures executing
/// `process_article` into the thread pool that is passed in.
fn process_feed(
    url: &str,
    index: Arc<Mutex<ArticleIndex>>,
    urls: Arc<Mutex<HashSet<String>>>,
    sites_pool: Arc<Mutex<ThreadPool>>,
) -> RssIndexResult<()> {
    let contents = reqwest::blocking::get(url)?.bytes()?;
    let channel = Channel::read_from(&contents[..])?;
    let items = channel.into_items();

    for item in items {
        let urls = Arc::clone(&urls);
        let index = Arc::clone(&index);

        sites_pool.lock().unwrap().execute(move || {
            let url_option = item.link();
            match url_option {
                Some(url) => {
                    let title_option = item.title();
                    match title_option {
                        Some(title) => {
                            let site_result = Url::parse(&url);
                            match site_result {
                                Ok(site_option) => match site_option.host_str() {
                                    Some(site_1) => {
                                        let site = site_1.to_string();
                                        if urls.lock().unwrap().contains(url) {
                                            println!(
                                                "Skipping already seen article: {} [{}]",
                                                title, url
                                            );
                                            return;
                                        }
                                        urls.lock().unwrap().insert(url.to_string());

                                        println!("Processing article: {} [{}]", title, url);

                                        let article =
                                            Article::new(url.to_string(), title.to_string());
                                        let article_words =
                                            process_article(&article).unwrap_or_default();
                                        index.lock().unwrap().add(
                                            site.to_string(),
                                            title.to_string(),
                                            url.to_string(),
                                            article_words,
                                        );

                                        return ();
                                    }
                                    None => return (),
                                },
                                Err(_) => return (),
                            };
                        }
                        None => return (),
                    }
                }
                None => return (),
            };
        });
    }

    Result::Ok(())
}
