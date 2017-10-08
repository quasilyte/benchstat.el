;;; benchstat.el --- proper benchmarking made simple

;; Author: Iskander Sharipov <quasilyte@gmail.com>
;; URL: https://github.com/Quasilyte/benchstat.el
;; Version: 0.8
;; Keywords: lisp

;;; Commentary:

;; This package makes proper benchmark results analysis far easier by
;; leveraging benchstat utility.
;;
;; One can use this package instead of `benchmark-run-compiled'
;; and get the benefits of correct (statistically significant),
;; easy-to-read profile stats.
;;
;; In orded to use this, you need benchstat binary:
;; https://godoc.org/golang.org/x/perf/cmd/benchstat
;;
;; For detailed documentation, please visit `benchstat.el'
;; github repository.

;;; Code:

;; Public:

(defgroup benchstat nil
  "Emacs Lisp benchmarking with benchstat."
  :group 'lisp)

(defcustom benchstat-program "benchstat"
  "Command that is used to invoke `benchstat' utility."
  :group 'benchstat)

(defcustom benchstat-buffer "*benchstat*"
  "The name of temporary output buffer for benchstat compare functions."
  :group 'benchstat)

(defcustom benchstat-filename-new
  (expand-file-name "benchstat-new"
                    temporary-file-directory)
  "File that is used to collect `new' code profile."
  :group 'benchstat)

(defcustom benchstat-filename-old
  (expand-file-name "benchstat-old"
                    temporary-file-directory)
  "File that is used to collect `old' code profile."
  :group 'benchstat)

(defvar benchstat-run-count 10
  "Default run count for benchstat/run functions.
The value of lower than 5 is unrecommended.
The value higher than 10 can be good, but most of the time it's redundant.")

(defun benchstat-compare (&optional new old delta-test)
  "Compare profiles from NEW and OLD files by using the DELTA-TEST method.
By default:
  - NEW set to `benchstat-filename-new'
  - OLD set to `benchstat-filename-old'
  - DELTA-TEST set to \"utest\" (two-sample Welch)

The alternative DELTA-TEST is \"ttest\" (two-sample Welch).

If you want to specify only DELTA-TEST, use:
\(benchstat-compare nil nil \"ttest\"\)

You can compare older profiles by using explicit NEW and OLD."
  (interactive)
  (let ((new (or new benchstat-filename-new))
        (old (or old benchstat-filename-old))
        (delta-test (or delta-test "utest")))
    (with-output-to-temp-buffer benchstat-buffer
      (shell-command
       (format "%s -delta-test=%s %s %s" benchstat-program delta-test new old)
       benchstat-buffer))))

(defun benchstat-reset (tag)
  "Clear profile data for TAG."
  (with-temp-file (benchstat--file tag)
    (insert "")))

(defmacro benchstat-run (TAG REPETITIONS &rest FORMS)
  "Using TAG profile, run REPETITIONS timed evaluations of FORMS.
Internally, uses `benchmark-run-compiled' to collect profile data.

Overwrites profile data file.
Use `benchstat-run-more' if you want to append instead."
  (declare (indent 2))
  (let ((file (benchstat--file TAG)))
    `(with-temp-file ,file
       (insert (benchstat--run ,REPETITIONS ,@FORMS)))))

(defmacro benchstat-run-more (TAG REPETITIONS &rest FORMS)
  "Like `benchstat-run', but does not overwrite profile data file."
  (declare (indent 2))
  (let ((file (benchstat--file TAG)))
    `(with-temp-buffer
       (insert "\n")
       (insert (benchstat--run ,REPETITIONS ,@FORMS))
       (append-to-file nil nil ,file))))

;; Private:

(defmacro benchstat--run-once (REPETITIONS &rest FORMS)
  "Return benchstat-compatible line for REPETITIONS execution of FORMS.
Benchmarking is done via `benchmark-run-compiled'."
  (when (symbolp REPETITIONS)
    (setq REPETITIONS (symbol-value REPETITIONS)))
  (unless (natnump REPETITIONS)
    (error "REPETITIONS is expected to be a positive number (got %S)"
           REPETITIONS))
  `(benchstat--format ,REPETITIONS
                      (benchmark-run-compiled ,REPETITIONS ,@FORMS)))

(defmacro benchstat--run (REPETITIONS &rest FORMS)
  "Execute `benchstat--run-once' for `benchstat-run-count' times."
  `(let ((run-count benchstat-run-count)
         lines)
     (dotimes (i run-count)
       (with-temp-message (format "benchstat: run %d/%d" i run-count)
         (push (benchstat--run-once ,REPETITIONS ,@FORMS)
               lines)))
     (mapconcat #'identity lines "\n")))

(defun benchstat--format (n stats)
  "Convert `(benchmark-run N ...)' result, STATS, to benchstat format."
  (pcase-let* ((`(,time-s ,gc-runs) stats)
               ;; Pedantically speaking, multiplier should be 10
               ;; times higher, but it will lead to less
               ;; pleasant results.
               ;; We are interested in relative
               ;; differences between `old' and `new', not in
               ;; the absolute values of `time-ns'.
               ;; Who measures Emacs Lisp execution in nanoseconds, anyway?
               (time-ns (* 100000000.0 time-s)))
    (when (< time-s 0)
      (display-warning 'benchstat
                       (format-message "Negative execution time: %fs (%dns)"
                                       time-s
                                       time-ns)
                       :warning))
    (format "BenchmarkEmacs %d %d ns/op %d allocs/op"
            n time-ns gc-runs)))

(defun benchstat--file (tag)
  "Return profile filename for specified TAG."
  (unless (memq tag '(:old :new))
    (error "TAG should be either `:old' or `:new' (have `%S')" tag))
  (if (eq :old tag)
      benchstat-filename-old
    benchstat-filename-new))

(provide 'benchstat)

;;; benchstat.el ends here
