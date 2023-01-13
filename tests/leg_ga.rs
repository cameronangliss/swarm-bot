use swarm_bot::leg_ga::LegParams;

#[test]
fn leg_ga() {
    let params = LegParams {
        pop_size: 20,
        num_neurons: 5,
        iters: 50,
        mut_rate: 1e-3,
        seed: 1,
    };
    let mut records = params.init_leg_records();
    let legs = records.legs.clone();
    assert_eq!(legs.len(), 20);
    records.compute_leg_ga(&params, 1);
    assert_eq!(records.legs.len(), 20);
    assert_ne!(legs, records.legs);
}
