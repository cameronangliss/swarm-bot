use std::fs;
use std::io::{stdin, stdout, Write};
use std::time::Instant;

use swarm_bot::bot_ga::{BotParams, BotRecords};
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
    print!("\nLoad a saved run? (y/n): ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    match input.trim() {
        "y" => bot_load(),
        "n" => bot_init(),
        _ => {
            println!("Invalid input. Try again.");
            bot_main()
        }
    }
}

fn bot_init() {
    let params = request_bot_params();
    print!("\nInitializing generation 0...");
    stdout().flush().unwrap();
    let start = Instant::now();
    fastrand::seed(params.seed);
    let mut records = params.init_bot_records();
    let elapsed = start.elapsed();
    println!("done! Elapsed time: {}s.", elapsed.as_secs());
    bot_view(&params, &mut records)
}

fn bot_load() {
    let params = request_bot_params();
    print!("\nLoading bot data...");
    stdout().flush().unwrap();
    let start = Instant::now();
    let filepath = format!("runs/{}.txt", params.label());
    let save_str = fs::read_to_string(filepath).unwrap();
    let (mut records, seed): (BotRecords, u64) = serde_json::from_str(&save_str).unwrap();
    fastrand::seed(seed);
    let elapsed = start.elapsed();
    println!(
        "done! Elapsed time: {}s, mut_rate: {}",
        elapsed.as_secs(),
        records.mut_rate
    );
    bot_view(&params, &mut records)
}

fn bot_view(params: &BotParams, records: &mut BotRecords) {
    println!(
        "\nComputed {} generations. What would you like to do?",
        records.max_fits.len() - 1
    );
    println!("Options:");
    println!("    maxbot");
    println!("    run");
    println!("    plotEvo");
    println!("    plotMax");
    println!("    back");
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    match input.trim() {
        "maxbot" => {
            println!("{:?}", records.max_bots.iter().last().unwrap());
            bot_view(params, records)
        }
        "run" => {
            print!("What generation to compute up to: ");
            stdout().flush().unwrap();
            let mut gen_str = String::new();
            stdin().read_line(&mut gen_str).unwrap();
            bot_run(gen_str.trim().parse().unwrap(), params, records)
        }
        "plotEvo" => records.plot(params, "evo"),
        "plotMax" => records.plot(params, "max"),
        "back" => main(),
        _ => {
            println!("Invalid input. Try again.");
            bot_view(params, records)
        }
    }
}

fn bot_run(gens: usize, params: &BotParams, records: &mut BotRecords) {
    while records.best_fits.len() < gens {
        print!(
            "Computing up to {}th generation...",
            records.best_fits.len() + 999
        );
        stdout().flush().unwrap();
        let start = Instant::now();
        records.compute_bot_ga(params, 1000);
        records.plot(params, "default");
        records.save(params);
        let elapsed = start.elapsed();
        println!(
            "done! Elapsed time: {}s, mut_rate: {}",
            elapsed.as_secs(),
            records.mut_rate
        );
    }
    bot_view(params, records)
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

    print!("    min_std_dev: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let min_std_dev = input.trim().parse().unwrap();

    print!("    sample_size: ");
    stdout().flush().unwrap();
    let mut input = String::new();
    stdin().read_line(&mut input).unwrap();
    let sample_size = input.trim().parse().unwrap();

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
        min_std_dev,
        sample_size,
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
    loop {
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
