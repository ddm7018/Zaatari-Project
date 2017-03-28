$("[data-value='Dimension Builder']")[1].children[0].style.width = "400px";
$("[data-value='Dimension Builder']")[1].children[1].style.width = "400px";
$("[data-value='Dimension Builder']")[1].children[5].style.paddingTop = "50px";

function waitSeconds(iMilliSeconds) {
    var counter= 0
        , start = new Date().getTime()
        , end = 0;
    while (counter < iMilliSeconds) {
        end = new Date().getTime();
        counter = end - start;
    }
}


var element = document.getElementById("addDim");
element.addEventListener("click", function(e) { 
  var addDimVal = $("[data-value='Dimension Builder']")[1].children[3].children[1].value;
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
    alert("User can only store ten dimensions, please delete some");
  }
  else if(RegExp(/\W+/g).exec($("[data-value='Dimension Builder']")[1].children[3].children[1].value) !== null){
    alert("Dimension names must be letters, numbers or underscores.");
  }
  
  else{
    alert("Adding Dimension " + addDimVal);
    waitSeconds(3000);
    document.getElementsByTagName("li")[0].children[0].click(); 
  }}, false);
  
var element = document.getElementById("dltFunc");
element.addEventListener("click", function(e) { 
  var list1 = $("[data-value='Dimension Builder']")[1].children[5].children[0].children[3].children[1].children;
  var anyToDelete = false;
  for (i = 0; i < list1.length; i++) { 
    var selectedBool = list1[i].className.endsWith("selected");
    if(selectedBool){
      var selectedVal = list1[i].children[1].innerHTML;
      var localStorageVal = "shinyStore-ex1\\".concat(selectedVal);
      localStorage.removeItem(localStorage.key(localStorageVal));
      alert("Removing Dimension " + selectedVal);
      anyToDelete = true;
    }
  }
  if(anyToDelete){
  location.reload();
  }
  else{
    alert("Nothing to delete");
  }
  
  }, false);

