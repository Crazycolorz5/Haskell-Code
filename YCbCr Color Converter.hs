
-- Underlying constants from RGB expression we want to stick to monitor.
kb = 0.114 -- Experiment a bit!
kr = 0.299 -- TODO: Make formulae dynamic based on these values.

r'D y' cR cB = (298.082*y')/256                    + (408.583*cR)/256 - 222.921
g'D y' cR cB = (298.082*y')/256 - (100.291*cB)/256 - (208.120*cR)/256 + 135.576
b'D y' cR cB = (298.082*y')/256 + (516.412*cB)/256                    - 276.836

yCbCrToRGB::Double->Double->Double->(Double, Double, Double)
yCbCrToRGB y' cR cB = (r'D (y'*256) (cR*256) (cB*256), g'D (y'*256) (cR*256) (cB*256), b'D (y'*256) (cR*256) (cB*256))