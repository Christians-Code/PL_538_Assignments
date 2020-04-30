use std::sync::{mpsc, Arc, Mutex};
use std::thread;

/// Message type to communicate with workers. A JobMsg is either a FnOnce closure or None, which
/// signals the worker to shut down.
type JobMsg = Option<Box<dyn FnOnce() + Send + 'static>>;

/// A ThreadPool should have a sending-end of a mpsc channel (`mpsc::Sender`) and a vector of
/// `JoinHandle`s for the worker threads.
pub struct ThreadPool {
    sender: mpsc::Sender<JobMsg>,
    workers: Vec<thread::JoinHandle<()>>,
}

impl ThreadPool {
    /// Spin up a thread pool with `num_workers` threads. Workers should all share the same
    /// receiving end of an mpsc channel (`mpsc::Receiver`) with appropriate synchronization. Each
    /// thread should loop and (1) listen for new jobs on the channel, (2) execute received jobs,
    /// and (3) quit the loop if it receives None.
    pub fn new(num_workers: usize) -> Self {
        let (tx, rx): (mpsc::Sender<JobMsg>, _) = mpsc::channel();

        let rx = Arc::new(Mutex::new(rx));

        let mut vec: Vec<thread::JoinHandle<()>> = Vec::new();

        for _ in 0..num_workers {
            let rx = Arc::clone(&rx);

            let handle = thread::spawn(move || loop {
                let received = rx.lock().unwrap().recv();
                match received {
                    Ok(msg) => {
                        match msg {
                            Some(job) => {
                                job();
                            }
                            None => break,
                        };
                    }
                    Err(_) => {}
                };
            });

            vec.push(handle);
        }

        ThreadPool {
            sender: tx,
            workers: vec,
        }
    }

    /// Push a new job into the thread pool.
    pub fn execute<F>(&mut self, job: F)
    where
        F: FnOnce() + Send + 'static,
    {
        self.sender.send(Some(Box::new(job))).unwrap();
    }
}

impl Drop for ThreadPool {
    /// Clean up the thread pool. Send a kill message (None) to each worker, and join each worker.
    /// This function should only return when all workers have finished.
    fn drop(&mut self) {
        let total_workers = self.workers.len();

        for _ in 0..total_workers {
            self.sender.send(None).unwrap();
        }

        for _ in 0..total_workers {
            if let Some(worker) = self.workers.pop() {
                worker.join().unwrap();
            }
        }
    }
}
