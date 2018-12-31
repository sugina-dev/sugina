"use strict";

var saw = new Wad({
    source : 'square',
    volume : 3840,
    env : {
        attack : .01,
        decay : .005,
        sustain : .25,
        release : .8
    },
    filter : [{
        type : 'lowpass',
        frequency : 20,
        q : 0,
        env : {
            attack : .2,
            frequency : 10
        }
    }, {
        type : 'highpass',
        frequency : 1600,
        q : 0,
        env : {
            attack : .2,
            frequency : 800
        }
    }],
    tuna : {
        Overdrive : {
            outputGain : .5,
            drive : .1,
            curveAmount : 0,
            algorithmIndex : 3,
            bypass : .5
        }
    }
});

var NDictKey = {
    81 : "F3",  // Q
    87 : "G3",  // W
    69 : "A3",  // E
    82 : "B3",  // R

    84 : "C4",  // T
    89 : "D4",  // Y
    85 : "E4",  // U
    73 : "F4",  // I
    79 : "G4",  // O
    80 : "A4",  // P
    65 : "B4",  // A

    83 : "C5",  // S
    68 : "D5",  // D
    70 : "E5",  // F
    71 : "F5",  // G
    72 : "G5",  // H
    74 : "A5",  // J
    75 : "B5",  // K

    76 : "C6",  // L
    90 : "D6",  // Z
    88 : "E6",  // X
    67 : "F6",  // C
    86 : "G6",  // V
    66 : "A6",  // B
    78 : "B6",  // N

    77 : "C7"   // M
};

function receiveNote(e) {
    var keyCode = e.keyCode;

    var pitch = NDictKey[keyCode];
    var hold = !e.ctrlKey ? 0.375 : 0.75;

    if (pitch) {
        var pitchReal = e.shiftKey ? (pitch[0] + '#' + pitch[1]) : pitch;
        document.getElementById('musicInterface').innerHTML = "Key: " + pitchReal;
        saw.play({pitch : pitchReal, env : {hold : hold}});
    }

    e.preventDefault();
    return true;
};
