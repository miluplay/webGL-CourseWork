function main() {
    var canvas = document.getElementById('webgl');
    var gl = getWebGLContext(canvas);
    if (!gl) {
        console.log('Failed to get the rendering contexxt for WebGL');
        return;
    }
    // 在此处修改颜色
    gl.clearColor(0.5, 0.5, 0.0, 1.0);
    gl.clear(gl.COLOR_BUFFER_BIT);
}