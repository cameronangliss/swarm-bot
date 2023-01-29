const XS: [i16; 16] = [0, 1, 7, 16, 28, 41, 52, 63, 71, 75, 77, 76, 73, 73, 73, 73];
const YS: [i16; 16] = [0, 0, 3, 8, 14, 21, 28, 34, 39, 43, 45, 48, 49, 49, 49, 49];

#[derive(Clone)]
pub struct LegEnv {
    pub sens_values: [i16; 3],
    pub x_pos: i16,
    pub y_pos: i16,
    x_mom: i16,
    y_mom: i16,
}

impl Default for LegEnv {
    fn default() -> LegEnv {
        LegEnv {
            sens_values: [15, 0, 15],
            x_pos: 0,
            y_pos: 0,
            x_mom: 0,
            y_mom: 0,
        }
    }
}

impl LegEnv {
    pub fn act(&mut self, x_signal: usize, y_signal: usize) {
        self.update_momentum(x_signal, y_signal);
        self.update_position(x_signal, y_signal);
        self.sense();
    }

    fn sense(&mut self) {
        let sens_value1 = if self.x_pos == XS[0] { 15 } else { 0 };
        let sens_value2 = if self.x_pos == XS[15] { 15 } else { 0 };
        let sens_value3 = if self.y_pos == YS[0] { 15 } else { 0 };
        self.sens_values = [sens_value1, sens_value2, sens_value3];
    }

    fn update_momentum(&mut self, x_signal: usize, y_signal: usize) {
        self.x_mom = calc_mom(x_signal, XS, self.x_pos, self.x_mom);
        self.y_mom = calc_mom(y_signal, YS, self.y_pos, self.y_mom);
    }

    fn update_position(&mut self, x_signal: usize, y_signal: usize) {
        self.x_pos = calc_pos(x_signal, XS, self.x_pos, self.x_mom, 10.0);
        self.y_pos = calc_pos(y_signal, YS, self.y_pos, self.y_mom, 5.0);
    }
}

fn calc_mom(signal: usize, pos_list: [i16; 16], pos: i16, mom: i16) -> i16 {
    let request = pos_list[signal];
    let delta = request - pos;
    match mom {
        mom if mom.signum() != delta.signum() => delta.signum(),
        mom if mom.abs() == 3 => mom,
        mom if mom > 0 => mom + 1,
        mom if mom < 0 => mom - 1,
        _ => 0,
    }
}

fn calc_pos(signal: usize, pos_list: [i16; 16], pos: i16, mom: i16, dist_limit: f32) -> i16 {
    if mom == 0 {
        pos
    } else {
        let request = pos_list[signal];
        let delta = request - pos;
        let max_dist = (dist_limit * 2.0_f32.powf(mom.abs() as f32 - 3.0)).floor() as i16;
        if delta.abs() <= max_dist {
            request
        } else {
            pos + delta.signum() * max_dist
        }
    }
}
