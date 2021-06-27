let outputVert = 'precision highp float;\n' +
  '\n' +
  'attribute vec2 aPosition;\n' +
  '\n' +
  'varying vec2 vUv;\n' +
  '\n' +
  'void main() {\n' +
  '    gl_Position = vec4(aPosition, 0.0, 1.0);\n' +
  '    vUv = aPosition * 0.5 + 0.5;\n' +
  '}';

let outputFrag = 'precision highp float;\n' +
  '\n' +
  'uniform sampler2D uComputedImage;\n' +
  '\n' +
  'varying vec2 vUv;\n' +
  '\n' +
  'void main() {\n' +
  '    gl_FragColor = texture2D(uComputedImage, vUv);\n' +
  '}';