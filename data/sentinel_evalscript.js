//VERSION=3
function setup() {
  return {
    input: [{bands: ["B02", "B03", "B04", "B08", "SCL", "dataMask"]}],
    output: {bands: 6, sampleType: "FLOAT32"}
  };
}
function evaluatePixel(s) {
  return [s.B02, s.B03, s.B04, s.B08, s.SCL, s.dataMask];
}
