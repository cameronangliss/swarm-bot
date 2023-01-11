const XS: [i16; 16] = [0, 1, 7, 16, 28, 41, 52, 63, 71, 75, 77, 76, 73, 73, 73, 73];
const YS: [i16; 16] = [0, 0, 3, 8, 14, 21, 28, 34, 39, 43, 45, 48, 49, 49, 49, 49];

#[derive(Clone)]
pub struct LegEnv {
    pub sens_values: Vec<i16>,
    pub x_pos: i16,
    pub y_pos: i16,
    x_mom: i16,
    y_mom: i16,
}

impl Default for LegEnv {
    fn default() -> LegEnv {
        LegEnv {
            sens_values: vec![15, 0, 15],
            x_pos: 0,
            y_pos: 0,
            x_mom: 0,
            y_mom: 0,
        }
    }
}

impl LegEnv {
    pub fn act(&self, x_sig: usize, y_sig: usize) -> LegEnv {
        let x_mom = self.next_x_mom(x_sig);
        let y_mom = self.next_y_mom(y_sig);
        let x_pos = self.next_x_pos(x_sig, x_mom);
        let y_pos = self.next_y_pos(y_sig, y_mom);
        let sens_value1 = if x_pos == XS[0] { 15 } else { 0 };
        let sens_value2 = if x_pos == XS[15] { 15 } else { 0 };
        let sens_value3 = if y_pos == YS[0] { 15 } else { 0 };
        let sens_values = vec![sens_value1, sens_value2, sens_value3];
        LegEnv {
            sens_values,
            x_pos,
            y_pos,
            x_mom,
            y_mom,
        }
    }

    fn next_x_mom(&self, signal: usize) -> i16 {
        let request = XS[signal];
        let delta = request - self.x_pos;
        if self.x_mom.signum() != delta.signum() {
            if self.x_mom < 0 {
                1
            } else {
                -1
            }
        } else if request == self.x_pos {
            0
        } else if self.x_mom.abs() == 3 {
            self.x_mom
        } else if self.x_mom < 0 {
            self.x_mom - 1
        } else if self.x_mom > 0 {
            self.x_mom + 1
        } else if delta > 0 {
            1
        } else {
            -1
        }
    }

    fn next_y_mom(&self, signal: usize) -> i16 {
        let request = YS[signal];
        let delta = request - self.y_pos;
        if self.y_mom.signum() != delta.signum() {
            if self.y_mom < 0 {
                1
            } else {
                -1
            }
        } else if request == self.y_pos {
            0
        } else if self.y_mom.abs() == 3 {
            self.y_mom
        } else if self.y_mom < 0 {
            self.y_mom - 1
        } else if self.y_mom > 0 {
            self.y_mom + 1
        } else if delta > 0 {
            1
        } else {
            -1
        }
    }

    fn next_x_pos(&self, signal: usize, mom: i16) -> i16 {
        let request = XS[signal];
        let delta = request - self.x_pos;
        let max_dist = (10.0 * 2.0_f32.powf(mom.abs() as f32 - 3.0)).floor() as i16;
        let valid_request = if delta.abs() <= max_dist {
            request
        } else {
            self.x_pos + delta.signum() * max_dist
        };
        if self.x_mom.signum() == delta.signum() {
            valid_request
        } else {
            self.x_pos
        }
    }

    fn next_y_pos(&self, signal: usize, mom: i16) -> i16 {
        let request = YS[signal];
        let delta = request - self.y_pos;
        let max_dist = (5.0 * 2.0_f32.powf(mom.abs() as f32 - 3.0)).floor() as i16;
        let valid_request = if delta.abs() <= max_dist {
            request
        } else {
            self.y_pos + delta.signum() * max_dist
        };
        if self.y_mom.signum() == delta.signum() {
            valid_request
        } else {
            self.y_pos
        }
    }
}
