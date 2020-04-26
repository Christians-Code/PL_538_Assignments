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
    todo!()
}

/// Same as the single/multi threaded version, but using a thread pool. Push closures executing
/// `process_article` into the thread pool that is passed in.
fn process_feed(
    url: &str,
    index: Arc<Mutex<ArticleIndex>>,
    urls: Arc<Mutex<HashSet<String>>>,
    sites_pool: Arc<Mutex<ThreadPool>>,
) -> RssIndexResult<()> {
    todo!()
}
