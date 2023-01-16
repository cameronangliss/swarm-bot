use fastrand;
use std::io::{stdin, stdout, Write};
use std::time::Instant;

use swarm_bot::bot_ga::BotParams;
use swarm_bot::leg_ga::LegParams;

fn main() {
    println!("\nSelect GA type.");
    println!("Options:");
    println!("    bot");
    println!("    leg");
    println!("    exit");
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    match input.trim() {
        "bot" => bot_main(),
        "leg" => leg_main(),
        "exit" => println!("Exiting program."),
        _ => {
            println!("Invalid input. Try again.");
            main()
        }
    }
}

fn bot_main() {
    let params = request_bot_params();
    print!("\nInitializing generation 0...");
    stdout().flush().unwrap();
    let start = Instant::now();
    fastrand::seed(params.seed);
    let mut records = params.init_bot_records();
    let elapsed = start.elapsed();
    println!("done! Elapsed time: {}s.", elapsed.as_secs());
    while records.best_fits.iter().last().unwrap() < &700.0 {
        print!(
            "Computing up to {}th generation...",
            records.best_fits.len() + 999
        );
        stdout().flush().unwrap();
        let start = Instant::now();
        records.compute_bot_ga(&params, 1000);
        records.plot(&params, "default");
        let elapsed = start.elapsed();
        println!("done! Elapsed time: {}s.", elapsed.as_secs());
    }
    println!("{:?}", records.max_bots.iter().last().unwrap());
}

fn request_bot_params() -> BotParams {
    println!("\nPlease enter parameters for leg GA.");

    print!("    pop_size: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let pop_size = input.trim().parse().unwrap();

    print!("    num_neurons: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let num_neurons = input.trim().parse().unwrap();

    print!("    init_gens: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let init_gens = input.trim().parse().unwrap();

    print!("    iters: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let iters = input.trim().parse().unwrap();

    print!("    mut_rate: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let mut_rate = input.trim().parse().unwrap();

    print!("    seed: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let seed = input.trim().parse().unwrap();

    BotParams {
        pop_size,
        num_neurons,
        init_gens,
        iters,
        mut_rate,
        seed,
    }
}

fn leg_main() {
    let params = request_leg_params();
    print!("\nInitializing generation 0...");
    stdout().flush().unwrap();
    let start = Instant::now();
    fastrand::seed(params.seed);
    let mut records = params.init_leg_records();
    let elapsed = start.elapsed();
    println!("done! Elapsed time: {}s.", elapsed.as_secs());
    while records.best_fits.iter().last().unwrap() < &400.0 {
        print!(
            "Computing up to {}th generation...",
            records.best_fits.len() + 999
        );
        stdout().flush().unwrap();
        let start = Instant::now();
        records.compute_leg_ga(&params, 1000);
        records.plot(&params, "default");
        let elapsed = start.elapsed();
        println!("done! Elapsed time: {}s.", elapsed.as_secs());
    }
    println!("{:?}", records.best_legs.iter().last().unwrap());
}

fn request_leg_params() -> LegParams {
    println!("\nPlease enter parameters for leg GA.");

    print!("    pop_size: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let pop_size = input.trim().parse().unwrap();

    print!("    num_neurons: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let num_neurons = input.trim().parse().unwrap();

    print!("    iters: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let iters = input.trim().parse().unwrap();

    print!("    mut_rate: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let mut_rate = input.trim().parse().unwrap();

    print!("    seed: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let seed = input.trim().parse().unwrap();

    LegParams {
        pop_size,
        num_neurons,
        iters,
        mut_rate,
        seed,
    }
}
