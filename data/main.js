(function(){
  var urls = [
    'https://upload.wikimedia.org/wikipedia/commons/thumb/f/f1/Behind_an_Old_Factory.jpg/1280px-Behind_an_Old_Factory.jpg',
    'https://upload.wikimedia.org/wikipedia/commons/0/03/Usine_abandonn%C3%A9e.JPG',
    'https://upload.wikimedia.org/wikipedia/commons/3/3d/Dilapidated_Soviet_Factory_-_panoramio.jpg',
    'https://upload.wikimedia.org/wikipedia/commons/2/27/Rust_old_metal.jpg',     'https://upload.wikimedia.org/wikipedia/commons/c/c4/Metal_stair_in_the_Rocks_Sydney.jpg'
  ];
  var url = urls[~~(Math.random()*urls.length)];
  document.documentElement.style.backgroundImage = "url('" + url + "')";
}());
