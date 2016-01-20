/**
 * @author Benedict R. Gaster
 * @description
 *
 * Angular routed controls for:
 *
 *   create instruments
 *   creating a module for a given instrument
 *   editing a module for a given instrument
 */

//  setup angular with necessary dependencies
var app = angular.module(
    'project',
    ['restangular', 'ngRoute','ngAnimate', 'ui.bootstrap']);


/**
 * @service ModuleService 
 *
 * @description Simple service to get module * name from moduleId and
 * a list of modules.
 */
app.factory('ModuleService', function () {
        return {
            lookupModule: function(moduleId, modules) {
		for (i = 0; i < modules.length; i++) {
		    if (modules[i].id == moduleId) {
			return modules[i].name;
		    }
		}
		
		return "module not found";
	    }
	}
});

/**
 * @description configure URL routes to:
 *
 * list.html
 *    support controller to enable listing all instruments
 *
 *    editinstrumentmodule.html
 *      edit a instrument's existing module
 *
 *    addinstrumentmodule.html
 *      create a new module for a given instrument
 *
 *    createInstrument.html
 *      create a new (empty, i.e. no modules) instrument
 */
app.config(function($routeProvider, RestangularProvider) {
    $routeProvider.
      when('/', {
        controller:ListInstrumentsCtrl,
        templateUrl:'list.html'
      }).
      when('/edit/:moduleName', {
        controller:EditInstrumentModuleCtrl,
        templateUrl:'editinstrumentmodule.html',
        resolve: {
          module: function(Restangular, $route) {
            var id = $route.current.params.moduleName;
            return Restangular.one('instrumentModules', id).get();
          }
        }
      }).
      when('/new/:instrumentId', {
          controller:CreateInstrumentModuleCtrl,
          templateUrl:'addinstrumentmodule.html',
	  resolve: {
              instId: function(Restangular, $route) {
		  return $route.current.params.instrumentId;
              }
          }
      }).	
      when('/newInstrument', {
        controller:CreateInstrumentCtrl,
          templateUrl:'createInstrument.html',
	  resolve: {
              instId: function(Restangular, $route) {
		  return $route.current.params.instrumentId;
              }
          }
      }).	
      otherwise({redirectTo:'/'});

      // our Restful JSON db server is running here...
      RestangularProvider.setBaseUrl('http://localhost:3000/');
  });

/**
 * CreateInstrumentModuleCtrl
 * @description
 */
function CreateInstrumentModuleCtrl(
    $scope,
    ModuleService,
    $location,
    Restangular,
    instId) {

    // get the instrument name (for title, among other things)
    Restangular.one("instruments", instId).get().then(function(inst) {
	$scope.instrumentName = inst.name;
    });

    // initialize a "default" module, using specified instrument ID
    $scope.instrumentModule = { channel: 1, moduleId: 0, instrumentId : instId };

    // store the name of the selected module in button assocated with dropdown
    // initially no module selected and so simply call it "Module"
    $scope.buttonName = "Module";

    // request a list of all modules
    $scope.modules = Restangular.all("modules").getList().$object;

    // select (set) a given module
    $scope.selectModule = function(moduleId) {
	$scope.instrumentModule.moduleId = moduleId;
	Restangular.one("modules", moduleId).get().then(function(mod) {
	    $scope.buttonName = mod.name;
	});
    }

    // save module to teh database and route to /
    $scope.save = function() {
	Restangular.all('instrumentModules')
	    .post($scope.instrumentModule)
	    .then(function(instrumentModule) {
		$location.path('/list');
	    });
    }
}

/**
 * 
 * @function CreateInstrumentCtrl
 *
 * @description Support controller to create a new instrument
 */
function CreateInstrumentCtrl($scope, ModuleService, $location, Restangular) {
     // initialize a "default" instrument, initially no name is assigned
    $scope.instrument = { name: ""};

    $scope.save = function() {
	Restangular.all('instruments')
	    .post($scope.instrument)
	    .then(function(instrument) {
		$location.path('/list');
	    });
    }
}

/**
 * @function ListInstrumentsCtrl
 *
 * @description Support controller to list all instruments
 */
function ListInstrumentsCtrl($scope, ModuleService, $location, Restangular) {

    // expose service to lookup module name from ID
    $scope.getModuleName     = ModuleService.lookupModule;

    // list of all instruments
    $scope.instruments       = Restangular.all("instruments").getList().$object;

    // list of all instrument modules
    $scope.instrumentModules = Restangular.all("instrumentModules").getList().$object;

    // list of all modules
    $scope.modules           = Restangular.all("modules").getList().$object;

    // delete a given instrument from database
    $scope.deleteInstrument = function(instrumentId) {
	Restangular.one("instruments", instrumentId).remove().then(function() {
	    $location.path('/list');
	});
    }

    /**
     * @function $scope.createArduinoModuleCode
     * @description 
     */
    $scope.createArduinoModuleCode = function(moduleId) {
	Restangular.one("modules", moduleId).get().then(function(mod) {
	    
	    Restangular.all("moduleControllers").getList().then(function(cons) {
		var code = filePrefix;

		code += fileStartModule(moduleId);
		
		var controllers = cons.plain().filter(function(control) {
		    return control.moduleId == moduleId;
		});

		var left = controllers.length;

		controllers.forEach(function(control) {

		    code += fileController(
			control.id,
			control.type,
			control.pin1,
			control.pin2,
			control.pin3);
		    
		    if (left > 1) {
			code += ",\n";
			left--;
		    }
		});

		code += fileEndModule(mod.name) + "\n\n";
		
		code += filePostfix(mod.name);
		
		
		var blob =
		    new Blob(
			[code],
			{type: "text/plain;charset=utf-8"});
		
		saveAs(blob, createFileName(mod.name, moduleId));
		
	    });

	    $location.path('/');
	});
    }
}

/**
 * @function EditInstrumentModuleCtrl
 *
 * @description Support contoller to edit a module for a given instrument
 */
function EditInstrumentModuleCtrl(
    $scope,
    ModuleService,
    $location,
    Restangular,
    module) {

    // save a copy of the incoming module, needed for cancel case
    var original = module;

    // setup copy of the original to be updated, needed for save case
    $scope.module = Restangular.copy(original);

    // get instrument name for display in page
    Restangular.one("instruments", original.instrumentId).get().then(function(inst) {
	$scope.instrumentName = inst.name;
    });

    // get module name for display in page
    Restangular.one("modules", original.moduleId).get().then(function(mod) {
	$scope.moduleName = mod.name;
    });

    // delete original from database and route to /
    $scope.destroy = function() {
	original.remove().then(function() {
	    $location.path('/list');
	});
    };

    // sav updated module to database and route to /
    $scope.save = function() {
	$scope.module.put().then(function() {
	    $location.path('/');
	});
    };
}

