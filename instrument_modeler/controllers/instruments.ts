/**
 * @module instruments.ts
 * @author Benedict R. Gaster
 * @copyright Benedict R. Gaster 2016
 * @description express routing for instruments
 */

/// <reference path="../models/Instrument.ts" />

module InstrumentRoutes {

    declare var require: any;

    var MongoClient = require('mongodb').MongoClient
    var assert = require('assert')
    var ObjectId = require('mongodb').ObjectID

    var express = require('express');
    export var router = express.Router();
    
    /**
     * @route GET: instruments
     */
    router.get('/instruments', function (req, res) {
	var instrumentDB = new Instrument.DB(req.db)
	instrumentDB.finds(function (instruments) {
	    res.send(instruments);
	});
    });

    /**
     * @route GET: instruments:id
     */
    router.get('/instruments/*', function (req, res) {
	var instrumentDB = new Instrument.DB(req.db)
	instrumentDB.find(
	    ObjectId.createFromHexString(req.params[0]),
	    function (instrument) {
		res.send(instrument);
	    });
    });

    /**
     * @route /instrumentModules
     */
    router.get('/instrumentModule/*', function (req, res) {
	var instrumentDB = new Instrument.DB(req.db)
	instrumentDB.findWithUniqueModule(
	    ObjectId.createFromHexString(req.params[0]),
	    function (instrumentModule) {
		res.send(instrumentModule);
	    });
    });

// PUT:

   /**
     * @route PUT instrument module (update it)
     */
    router.put('/instrumentModule', function (req, res) {
	var instrumentDB = new Instrument.DB(req.db)
	instrumentDB.updateModule(
	    ObjectId.createFromHexString(req.body._id),
	    ObjectId.createFromHexString(req.body.modules[0]._id),
	    req.body.modules[0].channel,
	    () => { res.end('update instrument module') })
    });

// POST:

    /**
     * @route POST instruments (create new instrument)
     */
    router.post('/instruments', function (req, res) {
	var instrumentDB = new Instrument.DB(req.db)
	instrumentDB.create(req.body.name, () => { 
	    res.end('instrument added');
	});
    });

   router.post('/instrumentAddModule', function (req, res) {
       var instrumentDB = new Instrument.DB(req.db)
       instrumentDB.addModule(
	   ObjectId.createFromHexString(req.body.instrumentId),
	   req.body.channel,
	   ObjectId.createFromHexString(req.body.moduleId),
	   () => { res.end('module added'); });
    });


    // DELETE

    /**
     * @route DELETE instrument
     */
    router.delete('/instruments/*', function (req, res) {
	var instrumentDB = new Instrument.DB(req.db)
	instrumentDB.remove(
	    ObjectId.createFromHexString(req.body.id),
	    () => { res.end('instrument delete') });
    });
    
       /**
     * @route DELETE instrument module
     */
    router.delete('/instrumentModule', function (req, res) {
	var instrumentDB = new Instrument.DB(req.db)
	instrumentDB.removeModule(
	    ObjectId.createFromHexString(req.body._id),
	    ObjectId.createFromHexString(req.body.modules[0]._id),
	    () => { res.end('instrument module delete') });	
    });
}
