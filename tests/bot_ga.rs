use swarm_bot::bot_ga::{transpose, BotParams};

#[test]
fn leg_ga() {
    let params = BotParams {
        pop_size: 20,
        num_neurons: 5,
        init_gens: 10,
        iters: 50,
        mut_rate: 1e-3,
        min_std_dev: 25.0,
        sample_size: 1000,
        seed: 1,
    };
    let mut records = params.init_bot_records();
    let leg_lists = records.leg_lists.clone();
    assert_eq!(leg_lists.len(), 6);
    for legs in &leg_lists {
        assert_eq!(legs.len(), 20);
    }
    records.compute_bot_ga(&params, 1);
    assert_eq!(records.leg_lists.len(), 6);
    for legs in &records.leg_lists {
        assert_eq!(legs.len(), 20);
    }
    assert_ne!(leg_lists, records.leg_lists);
}

#[test]
fn test_transpose() {
    let matrix = vec![vec![1, 2, 3], vec![4, 5, 6]];
    let trans_matrix = vec![vec![1, 4], vec![2, 5], vec![3, 6]];
    assert_eq!(transpose(&matrix), trans_matrix);
}
