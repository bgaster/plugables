/**
 * @module controllers.ts
 * @author Benedict R. Gaster
 * @copyright Benedict R. Gaster 2016
 * @description express routing for controllers
 */

/// <reference path="../models/Controller.ts" />

module ControllerRoutes {

    declare var require: any;

    var MongoClient = require('mongodb').MongoClient
    var assert = require('assert')
    var ObjectId = require('mongodb').ObjectID

    var express = require('express');
    export var router = express.Router();

    // GET

    /**
     * @route GET: controllerTypes
     */
    router.get('/controllerTypes', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.findTypes(function (controllers) {
	    res.send(controllers)
	});
    })

    /**
     * @route GET: moduleControllers
     */
    router.get('/moduleControllers', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.finds(function (moduleControllers) {
	    res.send(moduleControllers)
	});
    })

    /**
     * @route GET: moduleControllers/id
     */
    router.get('/moduleControllers/*', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.find(
	    ObjectId.createFromHexString(req.params[0]),
	    function (moduleController) {
	    res.send(moduleController)
	});
    })

    /**
     * @route GET: controllersForModule/moduleId
     */
    router.get('/controllersForModule/*', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.findModuleControllers(
	    ObjectId.createFromHexString(req.params[0]),
	    function (moduleControllers) {
	    res.send(moduleControllers)
	});
    })


    // PUT:

    /**
     * @route GET: controllerTypes
     */
    router.get('/controllerTypes', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.findTypes(function (controllers) {
	    res.send(controllers)
	});
    })

    /**
     * @route GET: moduleControllers
     */
    router.get('/moduleControllers', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.finds(function (moduleControllers) {
	    res.send(moduleControllers)
	});
    })

    /**
     * @route GET: moduleControllers/id
     */
    router.get('/moduleControllers/*', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.find(
	    ObjectId.createFromHexString(req.params[0]),
	    function (moduleController) {
	    res.send(moduleController)
	});
    })

    /**
     * @route GET: controllersForModule/moduleId
     */
    router.get('/controllersForModule/*', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.findModuleControllers(
	    ObjectId.createFromHexString(req.params[0]),
	    function (moduleControllers) {
	    res.send(moduleControllers)
	});
    })


    // PUT:

    /**
     * @route PUT instrument module (update it)
     */
    router.put('/moduleControllers', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.update(
	    ObjectId.createFromHexString(req.body._id),
	    ObjectId.createFromHexString(req.body.type_id),
	    req.body.pin1, req.body.controlCommand1,
	    req.body.pin2, req.body.controlCommand2,
	    req.body.pin3, req.body.controlCommand3,
	    () => { res.end('update instrument module') })
    });

    // POST:

    /**
     * @route POST moduleController 
     * @description create new module controller
     */
    router.post('/moduleControllers', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	var c = req.body
	console.log(c)
	controllerDB.create(
	    c.module_id,
	    c.type_id,
	    c.name,
	    c.pin1, c.controlCommand1,
	    c.pin2, c.controlCommand2,
	    c.pin3, c.controlCommand3,
	    () => { 
		res.end('controller added');
	    });	
    });

    // DELETE:

    /**
     * @route DELETE module controller
     */
    router.delete('/moduleControllers', function (req, res) {
	var controllerDB = new Controller.DB(req.db)
	controllerDB.remove(
	    ObjectId.createFromHexString(req.body._id),
	    () => { res.end('module controller deleted') });
    });
}
