use swarm_bot::leg::make_rand_leg;

#[test]
fn leg_test() {
    let leg = make_rand_leg(5);
    let positions = leg.clone().test(100);
    assert_eq!(positions.len(), 101);
}
