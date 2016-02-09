/**
 * @author Benedict R. Gaster
 * @description
 *
 * Angular routed controls for:
 *
 *   create modules
 *   creating a controller for a given module
 *   editing a controller for a given module
 */

// setup angular with necessary dependencies
var app = angular.module(
    'project',
    ['restangular', 'ngRoute','ngAnimate', 'ui.bootstrap']);

/**
 * @description configure URL routes to:
 *
 *  list.html
 *      support controller to enable listing of all modules
 *
 *   editmodulecontroller.html
 *      edit a module's existing contorller
 *
 *  addmodulecontroller.html
 *      create a new controller for a given a module
 *
 *  createModule.html
 *      create a new (empty, i.e. not contollers) module
 *  
 */
app.config(function($routeProvider, RestangularProvider) {
    $routeProvider.
      when('/', {
        controller:ListModulesCtrl,
        templateUrl:'list.html'
      }).
      when('/edit/:moduleControllerId', {
          controller:EditModuleControllerCtrl,
        templateUrl:'editmodulecontroller.html',
        resolve: {
          control: function(Restangular, $route) {
            var id = $route.current.params.moduleControllerId;
            return Restangular.one('moduleControllers', id).get();
          }
        }
      }).
      when('/new/:moduleId', {
        controller:CreateModuleControllerCtrl,
          templateUrl:'addmodulecontroller.html',
	  resolve: {
             moduleId: function(Restangular, $route) {
		  return $route.current.params.moduleId;
              }
          }
      }).	
      when('/newModule', {
        controller:CreateModuleCtrl,
          templateUrl:'createModule.html',
	  resolve: {
              moduleId: function(Restangular, $route) {
		  return $route.current.params.moduleId;
              }
          }
      }).	
      otherwise({redirectTo:'/'});
      
    RestangularProvider.setBaseUrl('http://localhost:3000/');
    RestangularProvider.setDefaultHeaders({'Content-Type': 'application/json'});
  });

/**
 * @function CreateModuleControlCtrl
 * 
 * @description
 */
function CreateModuleControllerCtrl(
    $scope,
    $location,
    Restangular,
    moduleId) {

    Restangular.one("modules", moduleId).get().then(function(module) {
	$scope.moduleName = module.name;
    });

    $scope.typename = "notdefined";
    // initialize a "default" contoller, using specified module ID
    $scope.controller = {   "name": "notdefined",
			    "type_id": "notdefined",
			    "pin1": "N/A",
			    "pin2": "N/A",
			    "pin3": "N/A",
			    "controlCommand1": -1,
			    "controlCommand2": -1,
			    "controlCommand3": -1,
			    "module_id": moduleId
			};
    
    // controller types and pins
    $scope.types = Restangular.all("controllerTypes").getList().then(function(types) {
	var ts = [];
	for (var i = 0; i < types.length; i++) {
	    ts[i] = { type: types[i].name, type_id: types[i]._id };
	}
    	$scope.types = ts;
    });
    //$scope.pins = Restangular.all("board").getList().$object;

    Restangular.one("board").get().then(function(board) {
    	$scope.pins = board.analog_pins.concat(board.digital_pins);
    });

    // set the controller type
    $scope.setType = function(type, type_id) {
	$scope.typename = type;
	$scope.controller.type_id = type_id;
    }

    // set the controller's pin1
    $scope.setPin1 = function(pin) {
	$scope.controller.pin1 = pin;
    }

    // set the controller's pin2
    $scope.setPin2 = function(pin) {
	$scope.controller.pin2 = pin;
    }

    // set the controller's pin3
    $scope.setPin3 = function(pin) {
	$scope.controller.pin3 = pin;
    }

    // save the controller to the database and route to /
    $scope.save = function() {
	if ($scope.typename != "notdefined" &&
	    $scope.controller.type_id != "notdefined" &&
	    $scope.controller.pin1 != "N/A" &&
	   $scope.controller.controlCommand1 != "N/A") {
	    Restangular.all('moduleControllers')
		.post($scope.controller)
		.then(function(controller) {
		    $location.path('/list');
		});
	}
	else {
	    $location.path('/list');
	}
    }
}

/**
 * @function CreateModuleCtrl
 *
 * @description Create a new module
 */
function CreateModuleCtrl($scope, $location, Restangular) {
    $scope.module = { name: ""};

    $scope.save = function() {
	Restangular.all('modules')
	    .post($scope.module)
	    .then(function(module) {
		$location.path('/list');
	    });
    }
}

/**
 * @function ListModulesCtrl
 *
 * @description
 */
function ListModulesCtrl($scope, $location, Restangular) {
    $scope.modules = Restangular.all("modules").getList().$object;
    $scope.moduleControllers = Restangular.all("moduleControllers").getList().$object;
    $scope.types = Restangular.all("controllerTypes").getList().$object;

    $scope.lookupName = function (types, id) {
	for (var i = 0; i < types.length; i++) {
	    if (types[i]._id == id) {
		return types[i].name;
	    }
	}
    }
    
    $scope.deleteModule = function(moduleId) {
	Restangular.one("modules", moduleId).remove().then(function() {
	    $location.path('/list');
	});
    }
}

/**
 * @function EditModuleControllerCtrl
 *
 * @description
 */
function EditModuleControllerCtrl(
    $scope,
    $location,
    Restangular,
    control) {

    // read controller types and pins to populate form of corresponding types
//    $scope.types = Restangular.all("controllerTypes").getList().$object;

    // controller types and pins
    // FIXME: I'm missing something with angular and why we need to copy the array!
    $scope.types = Restangular.all("controllerTypes").getList().then(function(types) {
	var ts = [];
	for (var i = 0; i < types.length; i++) {
	    if (types[i]._id == control.type_id) {
		$scope.typename = types[i].name;
	    }
	    ts[i] = { type: types[i].name, type_id: types[i]._id };
	}
    	$scope.types = ts;
    });

    // FIXME: add board selection on module edit field
    // at the moment we are only working with a single board so does not
    // matter :-)
    Restangular.one("board").get().then(function(board) {
    	$scope.pins = board.analog_pins.concat(board.digital_pins);
    });
    
    // save a copy of the incoming module, needed for cancel case
    var original = control;

    // setup up a copy of the original to be updated, need for save case
    $scope.controller = Restangular.copy(original);
    
    // get module name for display in page
    Restangular.one("modules", original.module_id).get().then(function(module) {
	$scope.moduleName = module.name;
    });

    // set controller type
    $scope.setType = function(type, type_id) {
	$scope.typename = type;
	$scope.controller.type_id = type_id;
    }

    // set controller pin 1
    $scope.setPin1 = function(pin) {
	$scope.controller.pin1 = pin;
    }

    // set controller pin 2
    $scope.setPin2 = function(pin) {
	$scope.controller.pin2 = pin;
    }

    // set controller pin 3
    $scope.setPin3 = function(pin) {
	$scope.controller.pin3 = pin;
    }

    // delete original from database and route to /
    $scope.destroy = function() {
	original.remove().then(function() {
	    $location.path('/list');
	});
    };

    // save updated controller and route to /
    $scope.save = function() {
	$scope.controller.put().then(function() {
	    $location.path('/');
	});
    };
}

