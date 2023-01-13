use fastrand;
use std::io::{stdout, Write};
use std::time::Instant;

use swarm_bot::bot_ga::BotParams;

fn main() {
    print!("\nInitializing generation 0...");
    stdout().flush().unwrap();
    let start = Instant::now();
    let params = BotParams {
        pop_size: 200,
        num_neurons: 5,
        init_gens: 100,
        iters: 100,
        mut_rate: 1e-3,
        seed: 1,
    };
    fastrand::seed(params.seed);
    let mut records = params.init_bot_records();
    let elapsed = start.elapsed();
    println!("done! Elapsed time: {}s.", elapsed.as_secs());
    while records.max_fits.iter().last().unwrap() < &700.0 {
        print!(
            "Computing up to {}th generation...",
            records.max_fits.len() + 999
        );
        stdout().flush().unwrap();
        let start = Instant::now();
        records = records.compute_bot_ga(&params, 1000);
        records.plot(&params, "default");
        let elapsed = start.elapsed();
        println!("done! Elapsed time: {}s.", elapsed.as_secs());
    }
    println!("{:?}", records.max_bots.iter().last().unwrap());
}
