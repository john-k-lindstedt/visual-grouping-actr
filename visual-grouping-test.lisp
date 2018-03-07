;load ACT-R
(load (merge-pathnames "../actr7/load-act-r.lisp" *load-truename*))

;(load (merge-pathnames "./visual-grouping.lisp" *load-truename*))

;load our task environment
(load (merge-pathnames "../VBEmulator/VBEmulator.lisp" *load-truename*))
(setf *vbw* (make-instance 'votebox-window :contest-lst cntst-lst))

;set up a model
(defvar *WHICH-DM* 'all-perfect)
(load (merge-pathnames "../models/Retrieve-Serial-Recog-Party-6.lisp" *load-truename*))
(install-device *vbw*)

;set parameters
(setf vg-grouping-radius 25)
(setf vg-grouping-type 'box) ;'point is faster, but less plausible (also needs a larger radius to work similarly)

;proc the display and check out the group values 
(proc-display)
(print-visicon)

;run almost not at all and reprocess display-- same display should have same groups
(run 0.01)
(proc-display)
(print-visicon)

(groups-to-file vg-scene (merge-pathnames "./data/test.tsv" *load-truename*))

;run a bit and process the display-- ought to have most, if not all, groups in commong
(run 20)
(proc-display)
(print-visicon)

;run the whole experiment... most groups should still be intact
(do-experiment)
(proc-display)
(print-visicon)