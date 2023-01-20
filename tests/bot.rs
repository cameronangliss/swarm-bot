use swarm_bot::bot::make_rand_bot;

#[test]
fn bot_test() {
    let bot = make_rand_bot(5);
    let position_lists = bot.clone().test(100);
    assert_eq!(position_lists.len(), 101);
    for positions in position_lists {
        assert_eq!(positions.len(), 6);
    }
}
