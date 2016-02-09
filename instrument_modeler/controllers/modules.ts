/**
 * @module modules.ts
 * @author Benedict R. Gaster
 * @copyright Benedict R. Gaster 2016
 * @description express routing for modules
 */

/// <reference path="../models/Module.ts" />

module ModuleRoutes {

    declare var require: any;

    var MongoClient = require('mongodb').MongoClient
    var assert = require('assert')
    var ObjectId = require('mongodb').ObjectID

    var express = require('express');
    export var router = express.Router();

    // GET

    /**
     * @route /modules
     */        
    router.get('/modules', function (req, res) {
	var moduleDB = new Module.DB(req.db)
	moduleDB.finds(function (modules) {
	    res.send(modules);
	});
    })

    /**
     * @route GET: modules:id
     */
    router.get('/modules/*', function (req, res) {
	var moduleDB = new Module.DB(req.db)
	moduleDB.find(
	    ObjectId.createFromHexString(req.params[0]),
	    function (mod) {
		res.send(mod);
	    });
    });

    // POST:

       /**
     * @route POST modules 
     * @description create new module
     */
    router.post('/modules', function (req, res) {
	var moduleDB = new Module.DB(req.db)
	moduleDB.create(req.body.name, () => { 
	    res.end('module added');
	});
    });

    // DELETE:
    
    /**
     * @route DELETE module
     */
    router.delete('/modules/*', function (req, res) {
	var moduleDB = new Module.DB(req.db)
	moduleDB.remove(
	    ObjectId.createFromHexString(req.params[0]),
	    () => { res.end('module delete') });
    });
    
}