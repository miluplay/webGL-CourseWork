var drawTriangle1 = function (color) {

  var triangleShape3 = new THREE.Shape();
  triangleShape3.moveTo(xposition, yposition);
  // 开始划线
  triangleShape3.lineTo(xposition - 20, yposition - 20);
  triangleShape3.lineTo(xposition + 20, yposition - 20);
  triangleShape3.lineTo(xposition, yposition);
  var geometry3 = new THREE.ShapeGeometry(triangleShape3);
  var material3 = new THREE.MeshBasicMaterial({
    color: color,
    side: THREE.DoubleSide
  });
  var cube3 = new THREE.Mesh(geometry3, material3);
  scene.add(cube3);

  function runder() {
    renderer.render(scene, camera);
    cube3.rotateZ(Math.PI / 200 * (direction));
    requestAnimationFrame(runder);
  }
  runder();
}