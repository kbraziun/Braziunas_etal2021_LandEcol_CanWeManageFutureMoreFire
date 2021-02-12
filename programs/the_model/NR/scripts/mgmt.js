/* management javascript for scenario runs
 * 
 * this script outlines defensible space management
 * for wui scenarios.
 * 
 * scripts are named by individual scenario because they 
 * reference different RU lists and/or maps.
 * 
*/

// global variables
var mgmt_list = undefined;
var mgmt_loaded = false;
var map = undefined;
var rumap = undefined;

/* load management map and rid list
* these will be loaded once, the first time management is called
*/

// create map once
function loadMap() {
	if (map == undefined) {
		map = Factory.newMap();
		map.load(Globals.path('gis/mgmt/scenario_map.txt'));
		print('map loaded');
	}
}

// load management rid list and reference map
function load() {
	mgmt_list = Factory.newCSVFile("");
	mgmt_list.loadFile(Globals.path('gis/mgmt/scenario_rids.csv'));
	print('mgmt_list with ' + mgmt_list.rowCount + ' rows loaded');
	rumap = Factory.newMap();
	rumap.load(Globals.path('gis/env_grid.txt'));
	print('ru map loaded');
}

// Determine species with maximum IV
// and assign appropriate crown shape parameters
function dom_species_crown(rid) {
	// load counts for each species
	var total_count = management.loadFromMap(rumap, rid);
	var total_ba = management.sum('basalarea');
	var abla_iv = management.sum('species=Abla')/total_count + 
		management.sum('basalarea','species=Abla')/total_ba;
	var pico_iv = management.sum('species=Pico or species=PicS')/total_count + 
		management.sum('basalarea','species=Pico or species=PicS')/total_ba;
	var pien_iv = management.sum('species=Pien')/total_count + 
		management.sum('basalarea','species=Pien')/total_ba;
	var potr_iv = management.sum('species=Potr')/total_count + 
		management.sum('basalarea','species=Potr')/total_ba;
	var psme_iv = management.sum('species=Psme')/total_count + 
		management.sum('basalarea','species=Psme')/total_ba;

	// create array with species and ivs
	// also populate with crown shape parameters
	var ivArr = [
		["Abla", abla_iv,0.2530,0.9778,2.2195],
		["Pico", pico_iv,0.2700,1.1194,2.7325],
		["Pien", pien_iv,0.2615,1.0486,2.4760],
		["Potr", potr_iv,0.3233,1.5627,4.338],
		["Psme", psme_iv,0.3015,1.3817,3.6825]
	];

	// loop through array to find species with maximum iv
	var maxIv = 0;
	var maxIndex = -1;

	for (var i=0; i < ivArr.length; i++) {
		if (ivArr[i][1] > maxIv) {
			maxIndex = i;
			maxIv = ivArr[i][1];
		}
	}

	return ivArr[maxIndex];

}

// Thin canopy fuels for a given rid
function thin_canopy(rid, tree_count) {
	if (tree_count > 0) {
		crownParams = dom_species_crown(rid);
		print("dominant species for rid " + rid + " is " + crownParams[0]);
		
		// calculate qmd
		var qmd = Math.sqrt((management.sum('basalarea')/tree_count) * (1/Math.PI)) * 200;
	
		// calculate target number of trees
		// max crown radius from purves equation plus buffer based on 
		// defensible space zone 1 recommended crown spacing
		var crownRadius = (crownParams[3] + (crownParams[4] - crownParams[3]) * qmd/40) + 5.5/2;
		// calculate crown area from this
		var crownArea = Math.PI * Math.pow(crownRadius,2);
		// calculate target tree density
		var target = 10000 / crownArea;
		// calculate number of trees to remove
		var to_remove = tree_count - target;
	
		print("qmd is " + qmd);
		print("starting density is " + tree_count);
		print("target is " + target);
	
		if (to_remove > 0) {
			// sort trees
			management.sort('dbh');
			// remove first from smallest 1/2 of trees
			var small_killed = management.managePct(0,50,to_remove);
			// remove remainded from largest 1/2 of trees
			var large_remove = to_remove - small_killed;
			var large_killed = management.managePct(0,100,large_remove);
			// calculate and print total removed
			var n_killed = small_killed + large_killed;
			print("removed " + n_killed + " trees");
		}
	} else {
		print("no trees in rid " + rid);
	}
}

/* callback function called by iLand.
 * executes management for each resource unit.
*/

function manage(year) {
	if (mgmt_loaded == false) {
		print("1st call - loading management...");
		load();
		loadMap();
		mgmt_loaded = true;
		print("loading management finished....");
	}

	if (year % 10 === 1) {
		print("executing management - year " + year);
		
		// use map to remove soil carbon and saplings
		management.loadFromMap(map, 1);
		// remove fuel pools. order is (map,id,swd,dwd,litter,som)
		management.removeSoilCarbon(map,1,0,0.95,0,0);
		management.killSaplings(map,1);

		// use rids to remove trees
		for (var i=0;i<mgmt_list.rowCount;i++) {
			// iterate through rids
			var rid = mgmt_list.value(i,0); 
			var tree_count = management.loadFromMap(rumap, rid);
			thin_canopy(rid, tree_count);
		}

		print('management complete')
		
	}
}