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
        self.calc_momentum(x_signal, y_signal);
        self.calc_position(x_signal, y_signal);
        self.sense();
    }

    fn sense(&mut self) {
        let sens_value1 = if self.x_pos == XS[0] { 15 } else { 0 };
        let sens_value2 = if self.x_pos == XS[15] { 15 } else { 0 };
        let sens_value3 = if self.y_pos == YS[0] { 15 } else { 0 };
        self.sens_values = [sens_value1, sens_value2, sens_value3];
    }

    fn calc_momentum(&mut self, x_signal: usize, y_signal: usize) {
        self.calc_x_mom(x_signal);
        self.calc_y_mom(y_signal);
    }

    fn calc_x_mom(&mut self, signal: usize) {
        let request = XS[signal];
        let delta = request - self.x_pos;
        self.x_mom = match self.x_mom {
            x_mom if x_mom.signum() != delta.signum() => delta.signum(),
            _ if delta == 0 => 0,
            x_mom if x_mom.abs() == 3 => x_mom,
            x_mom if x_mom > 0 => x_mom + 1,
            _ => self.x_mom - 1,
        };
    }

    fn calc_y_mom(&mut self, signal: usize) {
        let request = YS[signal];
        let delta = request - self.y_pos;
        self.y_mom = match self.y_mom {
            y_mom if y_mom.signum() != delta.signum() => delta.signum(),
            _ if delta == 0 => 0,
            y_mom if y_mom.abs() == 3 => y_mom,
            y_mom if y_mom > 0 => y_mom + 1,
            _ => self.y_mom - 1,
        };
    }

    fn calc_position(&mut self, x_signal: usize, y_signal: usize) {
        self.calc_x_pos(x_signal);
        self.calc_y_pos(y_signal);
    }

    fn calc_x_pos(&mut self, signal: usize) {
        if self.x_mom != 0 {
            let request = XS[signal];
            let delta = request - self.x_pos;
            let max_dist = (10.0 * 2.0_f32.powf(self.x_mom.abs() as f32 - 3.0)).floor() as i16;
            if delta.abs() <= max_dist {
                self.x_pos = request;
            } else {
                self.x_pos += delta.signum() * max_dist;
            }
        }
    }

    fn calc_y_pos(&mut self, signal: usize) {
        if self.y_mom != 0 {
            let request = YS[signal];
            let delta = request - self.y_pos;
            let max_dist = (5.0 * 2.0_f32.powf(self.y_mom.abs() as f32 - 3.0)).floor() as i16;
            if delta.abs() <= max_dist {
                self.y_pos = request;
            } else {
                self.y_pos += delta.signum() * max_dist;
            }
        }
    }
}
