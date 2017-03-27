$("[data-value='Function Builder']")[1].children[0].style.width = "500px";

var element = document.getElementById("addDim");
element.addEventListener("click", function(e) { 
  var addDimVal = $("[data-value='Function Builder']")[1].children[3].children[1].value;
  var doesExist = false;
  for (var key in localStorage){
   if(key.endsWith(addDimVal)){
     doesExist = true;
   }
  }
  if(doesExist){
    alert("Use a name that is not already taken");
  }
  else if(localStorage.length == 10){
    alert("User can only store ten dimensions");
  }
  else{
    alert("Adding Dim Val " + addDimVal);
    document.getElementsByTagName("li")[0].children[0].click(); 
  }}, false);
  
  
  
  

var element = document.getElementById("dltFunc");
element.addEventListener("click", function(e) { 
  var list1 = $("[data-value='Function Builder']")[1].children[5].children[0].children[3].children[1].children;
  for (i = 0; i < list1.length; i++) { 
    var selectedBool = list1[i].className.endsWith("selected");
    if(selectedBool){
      var selectedVal = list1[i].children[1].innerHTML;
      var localStorageVal = "shinyStore-ex1\\".concat(selectedVal);
      localStorage.removeItem(localStorage.key(localStorageVal));
      alert("Removing " + selectedVal);
    }
  }
  location.reload();
  
  }, false);

