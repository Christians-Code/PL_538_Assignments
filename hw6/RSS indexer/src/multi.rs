use rss::Channel;
use std::collections::{HashMap, HashSet};
use std::fs::File;
use std::io::BufReader;
use std::result::Result;

use std::sync::{Arc, Condvar, Mutex};
use std::thread;
use url::Url;

use crate::common::*;

/// Thread limits.
const MAX_THREADS_FEEDS: u32 = 5;
const MAX_THREADS_SITES: u32 = 10;
const MAX_THREADS_TOTAL: u32 = 18;

/// A lock around some T, with a condition variable for notifying/waiting.
struct CvarLock<T> {
    mutex: Mutex<T>,
    condvar: Condvar,
}

impl<T> CvarLock<T> {
    fn new(data: T) -> Self {
        let mutex = Mutex::new(data);
        let condvar = Condvar::new();
        CvarLock { mutex, condvar }
    }
}

/// Locks/Condvars around counters, tracking the number of feed threads, the number of article
/// threads per hostname, and the total number of threads.
pub struct ThreadCount {
    feeds_count: CvarLock<u32>,
    sites_count: CvarLock<HashMap<String, u32>>,
    total_count: CvarLock<u32>,
}

/// Same as for the single-threaded version, but now spawn a new thread for each call to
/// `process_feed`. Make sure to respect the thread limits!
pub fn process_feed_file(file_name: &str, index: Arc<Mutex<ArticleIndex>>) -> RssIndexResult<()> {
    let hash: HashMap<String, u32> = HashMap::new();

    let counters = Arc::new(ThreadCount {
        feeds_count: CvarLock::new(0),
        sites_count: CvarLock::new(hash),
        total_count: CvarLock::new(0),
    });

    let file = File::open(file_name)?;
    println!("Processing feed file: {}", file_name);

    let channel = Channel::read_from(BufReader::new(file))?;

    let urls = Arc::new(Mutex::new(HashSet::new()));

    let mut handles = Vec::<_>::new();

    for feed in channel.into_items() {
        let counters = Arc::clone(&counters);
        let urls = Arc::clone(&urls);
        let index = Arc::clone(&index);

        let handle = thread::spawn(move || {
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

                            {
                                let mut f_c = counters.feeds_count.mutex.lock().unwrap(); // try get lock
                                loop {
                                    let mut t_c = counters.total_count.mutex.lock().unwrap(); // try get lock
                                    if *f_c < MAX_THREADS_FEEDS && *t_c < MAX_THREADS_TOTAL {
                                        *f_c = *f_c + 1;
                                        *t_c = *t_c + 1;
                                        break;
                                    } else {
                                        std::mem::drop(t_c);
                                        f_c = counters.feeds_count.condvar.wait(f_c).unwrap();
                                    }
                                }
                            }

                            println!("Processing feed: {} [{}]", title, url);

                            process_feed(url, index, urls, counters).unwrap_or_default();

                        }
                        None => return,
                    }
                }
                None => return,
            };
        });

        handles.push(handle);
    }

    for handle in handles {
        handle.join();
    }

    Result::Ok(())
}

/// Same as for the single-threaded version, but now spawn a new thread for each call to
/// `process_article`. Make sure to respect the thread limits!
fn process_feed(
    url: &str,
    index: Arc<Mutex<ArticleIndex>>,
    urls: Arc<Mutex<HashSet<String>>>,
    counters: Arc<ThreadCount>,
) -> RssIndexResult<()> {
    let contents = reqwest::blocking::get(url)?.bytes()?;
    let channel = Channel::read_from(&contents[..])?;
    let items = channel.into_items();

    let mut handles = Vec::<_>::new();

    for item in items {
        let counters = Arc::clone(&counters);
        let urls = Arc::clone(&urls);
        let index = Arc::clone(&index);

        let handle = thread::spawn(move || {
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

                                        {
                                            let mut s_c =
                                                counters.sites_count.mutex.lock().unwrap(); // try get lock
                                            loop {
                                                let mut t_c =
                                                    counters.total_count.mutex.lock().unwrap(); // try get lock

                                                s_c.entry(site.clone()).or_insert(0);

                                                if let Some(x) = s_c.get_mut(&site) {
                                                    if *x < MAX_THREADS_SITES
                                                        && *t_c < MAX_THREADS_TOTAL
                                                    {
                                                        *x = *x + 1;
                                                        *t_c = *t_c + 1;
                                                        break;
                                                    } else {
                                                        std::mem::drop(t_c);
                                                        s_c = counters.sites_count.condvar.wait(s_c).unwrap();
                                                    }
                                                }
                                            }
                                        }

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

                                        {
                                            let mut t_c =
                                                counters.total_count.mutex.lock().unwrap(); // try get lock
                                            let mut s_c =
                                                counters.sites_count.mutex.lock().unwrap(); // try get lock
                                            if let Some(x) = s_c.get_mut(&site) {
                                                *x = *x - 1;
                                                *t_c = *t_c - 1;
                                            }
                                            counters.sites_count.condvar.notify_all();
                                        }
                                    }
                                    None => return,
                                },
                                Err(_) => return,
                            };
                        }
                        None => return,
                    }
                }
                None => return,
            };
        });

        handles.push(handle);
        //*f_c = *f_c - 1;
        //counters.feeds_count.condvar.notify_all();
    }
    {
        let mut t_c = counters.total_count.mutex.lock().unwrap(); // try get lock
        let mut f_c = counters.feeds_count.mutex.lock().unwrap(); // try get lock
        *f_c = *f_c - 1;
        *t_c = *t_c - 1;
        counters.feeds_count.condvar.notify_all();
    }

    for handle in handles {
        handle.join();
    }



    Result::Ok(())
}
