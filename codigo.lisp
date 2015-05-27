(in-package :user)

(defstruct jsp
  machines
  jobs)

(defun create-jsp (jsproblem)
  (make-jsp :machines (make-list (job-shop-problem-n.machines jsproblem) :initial-element nil)
            :jobs (copy-jobs (job-shop-problem-jobs jsproblem))))
            
(defun copy-jsp-prob (jsp-prob)
  (make-jsp :machines (copy-machines (jsp-machines jsp-prob))
            :jobs (copy-jobs (jsp-jobs jsp-prob))))

(defun copy-machines (machines)
  (let ((machines-aux))
    (dolist (machine machines machines-aux)
      (if (null machine)
          (setf machines-aux (append machines-aux (list nil)))
        (setf machines-aux (append machines-aux (list (copy-task machine))))))))
         
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

(defun makespan (jsp-prob)
  (let ((makespan 0))
    (dolist (machine (jsp-machines jsp-prob) makespan)
      (let ((end-time (get-task-end-time machine)))
        (if (and (numberp end-time)
                 (< makespan end-time))
              (setf makespan end-time))))))

(defun task-not-scheduled? (task)
  (null (job-shop-task-start.time task)))

(defun task-scheduled? (task)
  (not (task-not-scheduled? task)))

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

(defun get-task-end-time (task)
  (if (null task)
      nil
    (+ (job-shop-task-start.time task)
       (job-shop-task-duration task))))

(defun operator (state)
  (let ((successors nil)
        (state-aux nil)
        (task-aux nil))
    ;; Find the next unscheduled task for each job (just one task for each job)
    (dolist (job (jsp-jobs state) successors)
      (dolist (task (job-shop-job-tasks job)) 
        (when (task-not-scheduled? task)
          ;; The task isn't scheduled so we have to do it
          ;; To do that we have to create a new state
          (setf state-aux (copy-jsp-prob state))
          (setf task-aux (get-task (job-shop-task-job.nr task)
                                   (job-shop-task-task.nr task)
                                   (jsp-jobs state-aux)))
          
          ;;Check to see if it is the first task of the job
          (if (= (job-shop-task-task.nr task-aux) 0)
              ;;Check if the machine was already used
              (if (null (nth (job-shop-task-machine.nr task-aux) (jsp-machines state-aux))) 
                  
                  ;;First on job, First on machine
                  (progn
                    ;; Set the start time of the task to zero (it's the first task on that machine)
                    (setf (job-shop-task-start.time task-aux) 0)
                    ;; Add the task to the machines schedule
                    (setf (nth (job-shop-task-machine.nr task-aux) (jsp-machines state-aux)) task-aux)
                    (setf successors (append successors (list state-aux)))
                    (return))
                
                ;; First on job
                (progn
                  ;; Set the start time of the task to the end of the last task on the machine
                  (setf (job-shop-task-start.time task-aux)
                    (get-task-end-time (nth (job-shop-task-machine.nr task-aux) (jsp-machines state-aux))))
                  ;; Add the task to the machines schedule
                  (setf (nth (job-shop-task-machine.nr task-aux) (jsp-machines state-aux)) task-aux)
                  (setf successors (append successors (list state-aux)))
                  (return)))
            
            
            ;;Check the latest end time between the last task on the machine and the last task of the job
            
            
            ;;It's not the firt task of the job!
            (if (null (nth (job-shop-task-machine.nr task) (jsp-machines state-aux))) 
                
                ;; First on machine
                (progn
                  ;; Set the start time of the task to the end of the last task of the job
                  (setf (job-shop-task-start.time task-aux)
                    (get-task-end-time (get-task (job-shop-task-job.nr task-aux) 
                                                    (- (job-shop-task-task.nr task-aux) 1) 
                                                    (jsp-jobs state-aux))))
                  ;; Add the task to the machines schedule
                  (setf (nth (job-shop-task-machine.nr task-aux) (jsp-machines state-aux)) task-aux)
                  (setf successors (append successors (list state-aux)))
                  (return))
              
              
              ;; Check machine end-time and last task end-time
              (if (< (get-task-end-time (nth (job-shop-task-machine.nr task-aux) (jsp-machines state-aux)))
                     (get-task-end-time (get-task (job-shop-task-job.nr task-aux) 
                                                  (- (job-shop-task-task.nr task-aux) 1) 
                                                  (jsp-jobs state-aux))))
                  
                  ;; Set the start time of the task to the end of the last task of the job
                  (progn
                    (setf (job-shop-task-start.time task-aux)
                      (get-task-end-time (get-task (job-shop-task-job.nr task-aux) 
                                                   (- (job-shop-task-task.nr task-aux) 1) 
                                                   (jsp-jobs state-aux))))
                    ;; Add the task to the machines schedule
                    (setf (nth (job-shop-task-machine.nr task-aux) (jsp-machines state-aux)) task-aux)
                    (setf successors (append successors (list state-aux)))
                    (return))
                    
                    
                ;; Set the start time of the task to the end of the last task of the machine
                (progn
                  (setf (job-shop-task-start.time task-aux)
                    (get-task-end-time (nth (job-shop-task-machine.nr task-aux) (jsp-machines state-aux))))
                  ;; Add the task to the machines schedule
                  (setf (nth (job-shop-task-machine.nr task-aux) (jsp-machines state-aux)) task-aux)
                  (setf successors (append successors (list state-aux)))
                  (return))))))))))

;; Not usefull anymore (not guaranteed to work!)
(defun copy-problem (problem)
  (make-job-shop-problem
   :name (job-shop-problem-name problem)
   :n.jobs (job-shop-problem-n.jobs problem)
   :n.machines (job-shop-problem-n.machines problem)
   :jobs (copy-jobs problem)))

;; Deprecated, do not use!
(defun sort-jobs-by-duration (jobs &optional (order-by #'>))
  (sort (copy-list jobs) 
        order-by 
        :key (lambda (job) 
               (let ((value 0))
                 (dolist (task (job-shop-job-tasks job))
                   (setf value (+ value (job-shop-task-duration task))))
                 value))))

;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;
;; DAQUI PARA BAIXO NÃO ESTá TESTADO!!!!!
;;;;;;;;;;;;;;;;;;;;;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defun calendarizacao (problem search-type)
  ;; Assumimos um problema de estado completo representado
  ;; pela lista de todas as tasks a serem realizadas
  (let* ((jsp-prob (create-jsp problem)))))


