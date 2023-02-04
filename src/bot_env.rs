const BOT_WIDTH: f32 = 240.0;

#[derive(Default)]
pub struct BotEnv {
    x_pos: f32,
    y_pos: f32,
    dir: f32,
}

impl BotEnv {
    pub fn calc_path(&mut self, position_lists: &Vec<Vec<(i16, i16)>>) -> Vec<(f32, f32)> {
        let mut path = vec![(0.0, 0.0)];
        for i in 0..position_lists.len() - 1 {
            let leg_heights = position_lists[i + 1]
                .iter()
                .map(|(_, y)| *y)
                .collect::<Vec<_>>();
            let drag = get_drag_factor(&leg_heights);
            let leg_velocities_rel_to_bot = position_lists[i]
                .iter()
                .zip(position_lists[i + 1].iter())
                .map(|((x1, _), (x2, _))| x2 - x1)
                .collect::<Vec<_>>();
            let (thrust, deg_turn) = get_leg_effects(&leg_velocities_rel_to_bot, &leg_heights);
            self.dir += deg_turn;
            self.x_pos += drag * thrust * f32::cos(self.dir);
            self.y_pos += drag * thrust * f32::sin(self.dir);
            path.push((self.x_pos, self.y_pos));
        }
        path
    }
}

fn get_leg_effects(leg_velocities_rel_to_bot: &[i16], leg_heights: &[i16]) -> (f32, f32) {
    let leg_effectfulness_scores = leg_heights
        .iter()
        .map(|&height| {
            if height < 5 {
                let root = (29_f32.powf(0.5) - 5.0) / 2.0;
                1.0 / (height as f32 + root) - root
            } else {
                0.0
            }
        })
        .collect::<Vec<_>>();
    let left_scores = leg_effectfulness_scores.iter().step_by(2).copied();
    let right_scores = leg_effectfulness_scores.iter().skip(1).step_by(2).copied();
    let left_denom: f32 = left_scores.clone().sum();
    let right_denom: f32 = right_scores.clone().sum();
    let left_coeffs = left_scores.map(|score| {
        if left_denom == 0.0 {
            0.0
        } else {
            score / left_denom
        }
    });
    let right_coeffs = right_scores.map(|score| {
        if right_denom == 0.0 {
            0.0
        } else {
            score / right_denom
        }
    });
    let left_thrust = leg_velocities_rel_to_bot
        .iter()
        .step_by(2)
        .zip(left_coeffs)
        .fold(0.0, |acc, (&velocity, coeff)| acc + velocity as f32 * coeff);
    let right_thrust = leg_velocities_rel_to_bot
        .iter()
        .skip(1)
        .step_by(2)
        .zip(right_coeffs)
        .fold(0.0, |acc, (&velocity, coeff)| acc + velocity as f32 * coeff);
    let thrust = (left_thrust + right_thrust) / 2.0;
    let deg_turn = (right_thrust - left_thrust) / BOT_WIDTH;
    (thrust, deg_turn)
}

fn get_drag_factor(leg_heights: &[i16]) -> f32 {
    let on_ground = |ns: Vec<usize>| ns.iter().map(|&n| leg_heights[n]).all(|height| height == 0);
    let tri = on_ground(vec![0, 3, 4]) || on_ground(vec![1, 2, 5]);
    let rect = on_ground(vec![0, 1, 4, 5]);
    let par = on_ground(vec![0, 2, 3, 5]) || on_ground(vec![1, 2, 3, 4]);
    let static_stable = tri || rect || par;
    let num_on_ground = leg_heights.iter().filter(|height| **height == 0).count();
    let part_stable = on_ground(vec![0, 5]) || on_ground(vec![1, 4]) || on_ground(vec![2, 3]);
    if static_stable {
        1.0
    } else if part_stable {
        (num_on_ground + 6) as f32 / 16.0
    } else {
        num_on_ground as f32 / 16.0
    }
}
