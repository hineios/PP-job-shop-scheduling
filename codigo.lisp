(in-package :user)

(defstruct jsp
  machines
  jobs)

(defun copy-jsp-prob (jsp-prob)
  (make-jsp :machines (copy-tasks (jsp-machines jsp-prob))
            :jobs (copy-jobs (jsp-jobs jsp-prob))))

(defun copy-jobs (jobs)
  (let ((jobs-aux))
    (dolist (job jobs jobs-aux)
      (setf jobs-aux (append jobs-aux (list (copy-job job)))))))

(defun copy-job (job)
  (make-job-shop-job
   :job.nr (job-shop-job-job.nr job)
   :tasks (copy-tasks (job-shop-job-tasks job))))

(defun copy-tasks (tasks)
  (let ((tasks-aux nil))
    (dolist (task tasks tasks-aux)
      (setf tasks-aux (append tasks-aux (list (copy-task task)))))))

(defun copy-task (task)
  (make-job-shop-task
   :job.nr (job-shop-task-job.nr task)
   :task.nr (job-shop-task-task.nr task)
   :machine.nr (job-shop-task-machine.nr task)
   :duration (job-shop-task-duration task)
   :start.time (job-shop-task-start.time task)))

(defun extract-tasks (jsp-prob)
  "Extract a copy of the tasks from all jobs"
  (let ((tasks '()))
    (dolist (job (jsp-jobs jsp-prob) tasks)
      (setf tasks (append tasks (copy-tasks (job-shop-job-tasks job)))))))

;; Deprecated, do not use!
(defun sort-jobs-by-duration (jobs &optional (order-by #'>))
  (sort (copy-list jobs) 
        order-by 
        :key (lambda (job) 
               (let ((value 0))
                 (dolist (task (job-shop-job-tasks job))
                   (setf value (+ value (job-shop-task-duration task))))
                 value))))

(defun makespan (jsp-prob)
  (let ((makespan 0))
    (dolist (machine (jsp-machines jsp-prob) makespan)
      (if (and (not (null machine))
               (< makespan (+ (job-shop-task-start.time machine) (job-shop-task-duration machine))))
              (setf makespan (+ (job-shop-task-start.time machine) (job-shop-task-duration machine)))))))

(defun scheduled? (jsp-prob)
  (let ((finished? t))
    (dolist (job (jsp-jobs jsp-prob) finished?)
      (if (not finished?) (return nil))
      (dolist (task (job-shop-job-tasks job))
        (if (null (job-shop-task-start.time task)) (setf finished? nil))
        (if (not finished?) (return nil))))))

(defun get-last-task-scheduled-on (state machine.nr)
  (nth machine.nr (jsp-machines state)))

(defun get-task (job.nr task.nr jobs)
  (nth task.nr (job-shop-job-tasks (nth job.nr jobs))))

(defun get-end-time-of-task (task)
  (if (not (job-shop-task-start.time task))
      nil
    (+ (job-shop-task-start.time task)
       (job-shop-task-duration task))))



;; Not usefull anymore (not guaranteed to work!)
(defun copy-problem (problem)
  (make-job-shop-problem
   :name (job-shop-problem-name problem)
   :n.jobs (job-shop-problem-n.jobs problem)
   :n.machines (job-shop-problem-n.machines problem)
   :jobs (copy-jobs problem)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;; DAQUI PARA BAIXO NÃO ESTá TESTADO!!!!!
;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun calendarizacao (problem search-type)
  ;; Assumimos um problema de estado completo representado
  ;; pela lista de todas as tasks a serem realizadas
  (let* ((jsp-prob (make-jsp :machines (make-list (job-shop-problem-n.machines problem) :initial-element nil)
                         :jobs (copy-jobs (job-shop-problem-jobs problem)))))

    ))

(defun operator (state)
  (let ((successors nil)
        (state-aux nil)
        (last-task-end-time 0)
        (machine-end-time 0))
    ;; Find the next unscheduled task for each job (just one task for each job)
    (dolist (job (jsp-jobs state) successors)
      (dolist (task (job-shop-job-tasks job)) 
        (when (null (job-shop-task-start.time task))
          ;; The task isn't scheduled so we have to do it
          ;; To do that we have to create a new state
          (setf state-aux (copy-jsp state))
          
          ;;Check to see if it is the first task of the job
          (if (= (job-shop-task-task.nr task) 0) ;It's the first task of the job
              (if (null (nth (job-shop-task-machine.nr task) (jsp-machines state))) ;It's the first task on the machine
                  (progn
                    ;; Set the start time of the task of the new state to zero (it's the first task on that machine)
                    (setf (job-shop-task-start.time (get-task (job-shop-task-job.nr task)
                                                              (job-shop-task-task.nr task)
                                                              (jsp-jobs state-aux)))
                      0)
                    ;; Add the task to the machines schedule
                    (setf (nth (job-shop-task-machine.nr task) (jsp-machines state-aux)) 
                      (get-task (job-shop-task-job.nr task)
                                (job-shop-task-task.nr task)
                                (jsp-jobs state-aux))))
                
                ;; Else, it's not the first task on the machine
                (progn
                  ;; Set the start time of the task of the new state to the end of the last task on the machine
                  (setf (job-shop-task-start.time (get-task (job-shop-task-job.nr task)
                                                            (job-shop-task-task.nr task)
                                                            (jsp-jobs state-aux)))
                    (get-end-time-of-task (nth (job-shop-task-machine.nr task) (jsp-machines state-aux))))
                  ;; Add the task to the machines schedule
                  (setf (nth (job-shop-task-machine.nr task) (jsp-machines state-aux)) 
                    (get-task (job-shop-task-job.nr task)
                              (job-shop-task-task.nr task)
                              (jsp-jobs state-aux)))))
            
            ;;It's not the firt task of the job!
            ;;Check the latest end time between the last task on the machine and the last task of the job
            (if (null (nth (job-shop-task-machine.nr task) (jsp-machines state))) ;It's the first task on the machine
                (progn
                  ;; Set the start time of the task of the new state to the end of the last task of the job
                  (setf (job-shop-task-start.time (get-task (job-shop-task-job.nr task)
                                                            (job-shop-task-task.nr task)
                                                            (jsp-jobs state-aux)))
                    (get-end-time-of-task (get-task (job-shop-task-job.nr task) 
                                                    (- (job-shop-task-task.nr task) 1) 
                                                    (jsp-jobs state-aux))))
                  ;; Add the task to the machines schedule
                  (setf (nth (job-shop-task-machine.nr task) (jsp-machines state-aux)) 
                    (get-task (job-shop-task-job.nr task)
                              (job-shop-task-task.nr task)
                              (jsp-jobs state-aux))))
              
              (if (< (get-end-time-of-task (nth (job-shop-task-machine.nr task) (jsp-machines state-aux)))
                     (get-end-time-of-task (get-task (job-shop-task-job.nr task) 
                                                     (- (job-shop-task-task.nr task) 1) 
                                                     (jsp-jobs state-aux))))
                  (progn
                    ;; Set the start time of the task of the new state to the end of the last task of the job
                    (setf (job-shop-task-start.time (get-task (job-shop-task-job.nr task)
                                                              (job-shop-task-task.nr task)
                                                              (jsp-jobs state-aux)))
                      (get-end-time-of-task (get-task (job-shop-task-job.nr task) 
                                                      (- (job-shop-task-task.nr task) 1) 
                                                      (jsp-jobs state-aux))))
                    ;; Add the task to the machines schedule
                    (setf (nth (job-shop-task-machine.nr task) (jsp-machines state-aux)) 
                      (get-task (job-shop-task-job.nr task)
                                (job-shop-task-task.nr task)
                                (jsp-jobs state-aux))))
                (progn
                  ;; Set the start time of the task of the new state to the end of the last task on the machine
                  (setf (job-shop-task-start.time (get-task (job-shop-task-job.nr task)
                                                            (job-shop-task-task.nr task)
                                                            (jsp-jobs state-aux)))
                    (get-end-time-of-task (nth (job-shop-task-machine.nr task) (jsp-machines state-aux))))
                  ;; Add the task to the machines schedule
                  (setf (nth (job-shop-task-machine.nr task) (jsp-machines state-aux)) 
                    (get-task (job-shop-task-job.nr task)
                              (job-shop-task-task.nr task)
                              (jsp-jobs state-aux)))))))
                
          (setf successors (append successors (list state-aux))))))))
            
            
