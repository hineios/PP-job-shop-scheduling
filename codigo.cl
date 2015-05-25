(in-package :user)

(defstruct problem 
  job-shop-problem
  machines)
  
  
(defun calendarizacao (problem search-type)
  ;; Assumimos um problema de estado completo representado
  ;; pela lista de todas as tasks a serem realizadas
  (let* ((jobs (copy-list (job-shop-problem-jobs problem))))
    
    (sort-jobs-by-duration jobs #'<)
    (extract-tasks jobs)))

;; Print the jobs
;(dolist (job jobs)
; (format t "~A~%" job))
    

(defun extract-tasks (job-shop-jobs)
  "Extract the tasks from all jobs"
  (let ((solution '()))
    (dolist (job job-shop-jobs solution)
      (setf solution (append solution (job-shop-job-tasks job))))
    (dolist (task solution)
      (format t "~A~%" task))))


(defun sort-jobs-by-duration (jobs &optional (order-by #'>))
  (sort (copy-list jobs) 
        order-by 
        :key (lambda (job) 
               (let ((value 0))
                 (dolist (task (job-shop-job-tasks job))
                   (setf value (+ value (job-shop-task-duration task))))
                 value))))

(defun makespan (job-shop-prob)
  (let ((makespan 0)
        (solution? t))
    (dolist (job (job-shop-problem-jobs job-shop-prob) makespan)
      (if (not solution?) (return nil))
      (dolist (task (job-shop-job-tasks job))
        (if (not solution?) (return nil))
        (if (null (job-shop-task-start.time task))
            (progn 
              (setf solution? nil)
              (return nil))
          (if (< makespan (+ (job-shop-task-start.time task)
                             (job-shop-task-duration task)))
              (setf makespan (+ (job-shop-task-start.time task)
                                (job-shop-task-duration task)))))))))

(defun scheduled? (job-shop-prob)
  (let ((finished? t))
    (dolist (job (job-shop-problem-jobs job-shop-prob) finished?)
      (if (not finished?) (return nil))
      (dolist (task (job-shop-job-tasks job))
        (if (null (job-shop-task-start.time task)) (setf finished? nil))
        (if (not finished?) (return nil))))))

            


