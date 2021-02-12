/* spinup javascript
 * 
 * this script automates saving a snapshot at the end of the
 * 300-year spinup and also incorporates fire size selection
 * using calibrated values of rclim (referred to as KBDI
 * threshold in model calibration appendices). 
*/

// function to take snapshot at end of spinup
function snapshot() {
	if (Globals.year == 300) {
		Globals.saveModelSnapshot("init/spinup_300.sqlite");
	}
}

/* fire functions */

// draw number from negative exponential
function runln(mean) {
	var draw = -Math.log(Math.random());
	return draw*mean;
}

// function to draw from lower or higher distributions
function drawLower(mean, limit) {
	for (var i=0;i<100000;i++) {
		var draw = runln(mean);
		if (draw < limit && draw > 0) {
			return draw;
		}
	}
	return 0;
}

function drawUpper(mean, limit) {
	for (var i=0;i<100000;i++) {
		var drawu = runln(mean);
		if (drawu > limit && drawu > 0) {
			return drawu;
		}
	}
	return 0;
}


// calculate fire size; rclim=KBDI/KBDIRef, old_size=fire size drawn from standard distribution (iLand)

function onAfterCreate() {
	Fire.onCalculateFireSize = function(rclim, fire_size) {

		if (rclim <= 1.0) {
			draw = drawLower(200000, 100000);
			print("drawing from lower: " + draw);
			return draw;
		}

		if (rclim > 1.7) {
			drawu = drawUpper(12000000, 4000000);
			print("drawing from upper: " + drawu);
			return drawu;
		// drawn from distribution, applied min/max range
		}

		print("retaining original draw: " + fire_size);

		return fire_size;

	}

}

// fire outputs, useful for debugging

function afterFireProcessing() {
	var praefix = Fire.id;
	// save the form of the fire in ascii format, txt extension
	// save a file for each fire
	// Fire.gridToFile('spread', 'output/fire/spread' + praefix + '.txt');
	// Fire.gridToFile('basalarea', 'output/fire/basalarea' + praefix + '.txt');
	// Fire.gridToFile('crownkill', 'output/fire/ck' + praefix + '.txt');
	// Fire.grid('KBDI').save('output/fire/kbdi' + praefix + '.txt');
	// Fire.grid('fuel').save('output/fire/fuel' + praefix + '.txt');
	// Fire.grid('combustibleFuel').save('output/fire/combustibleFuel' + praefix + '.txt');
	Fire.grid('nFire').save('output/spinup_nfire.txt')
 
} 

/* function run at the end of every year */

function onYearEnd() {
	snapshot()
}