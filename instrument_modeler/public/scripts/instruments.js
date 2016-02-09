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
		    if (modules[i]._id == moduleId) {
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
      when('/edit/:uniqueModuleId', {
        controller:EditInstrumentModuleCtrl,
        templateUrl:'editinstrumentmodule.html',
        resolve: {
          instModule: function(Restangular, $route) {
              var id = $route.current.params.uniqueModuleId;
              return Restangular.one('instrumentModule', id).get();
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
    RestangularProvider.setDefaultHeaders({'Content-Type': 'application/json'});
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
    $scope.instrumentModule = { channel: 1, moduleId: "", instrumentId : instId };

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
	Restangular.all('instrumentAddModule')
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
    //$scope.instrumentModules = Restangular.all("instrumentModules").getList().$object;

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
     * FIXME: we need to do this for an instrument not just a module!!
     */
    $scope.createArduinoModuleCode = function(moduleId) {

	// query the module
	Restangular.one("modules", moduleId).get().then(function(mod) {

	    // query controller types
	    Restangular.all("controllerTypes").getList().then(function(types) {

		var arduino = new ArduinoCode(0, mod.name, types.plain());
		
		// query module controllers
		// FIXME: we should really just get the BE return
		//        controllers for module
		Restangular.one("moduleControllers").getList().then(function(cons) {

		    // filter only controllers associated with module
		    var controllers = cons.plain().filter(function(control) {
			return control.module_id == moduleId;
		    });
		    
		    var left = controllers.length;
		    
		    controllers.forEach(function(control) {
			
			arduino.pushController(
			    control.type_id,
			    control.pin1,
			    control.pin2,
			    control.pin3);		    
		    });
		    
		    var blob =
			new Blob(
			    [arduino.close()],
			    {type: "text/plain;charset=utf-8"});
		    
		    saveAs(blob, arduino.createFileName());
		    
		    $location.path('/');
		});
	    });
	});
    }

   /**
     * @function $scope.createLookupTables
     * @description 
     * FIXME: we need to do this for an instrument not just a module!!
     */
    $scope.createLookupTables = function(moduleId, channel) {

	var lookupTables = new LookupTables(channel);
	
	Restangular.one("modules", moduleId).get().then(function(mod) {

	    lookupTables.pushModule();
	    
	    Restangular.all("moduleControllers").getList().then(function(cons) {
		
		var controllers = cons.plain().filter(function(control) {
		    return control.module_id == moduleId;
		});

		controllers.forEach(function(control) {
		    lookupTables.pushControllers(
		     	control.controlCommand1,
		     	control.controlCommand2,
		     	control.controlCommand3);
		});

		var tables = lookupTables.closedTables();

		// we save as ASCII, no unicode encoding, simply as it
		// is not needed
		var blob = new Blob(
		    [tables],
		    {type: "text/plain;"});

		saveAs(blob, "lookupTable.txt");

		$location.path('/');
	    });
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
    instModule) {

    // save a copy of the incoming module, needed for cancel case
    var original = instModule;

    // setup copy of the original to be updated, needed for save case
    $scope.instModule = Restangular.copy(original);

    // get instrument name for display in page
    // Restangular.one("instruments", original.instrumentId).get().then(function(inst) {
    // 	$scope.instrumentName = inst.name;
    // });

    // get module name for display in page
    Restangular.one(
	"modules",
	original.modules[0].module_id).get().then(function(mod) {
	    
	$scope.moduleName = mod.name;
    });

    // delete original from database and route to /
    $scope.destroy = function() {
	original.remove().then(function() {
	    $location.path('/list');
	});
    };

    // save updated module to database and route to /
    $scope.save = function() {
	$scope.instModule.put().then(function() {
	    $location.path('/');
	});
    };
}

