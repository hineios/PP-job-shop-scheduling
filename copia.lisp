(in-package :user)

(defun copy-problem (problem)
  (let ((list-aux nil))
    (dolist (job (job-shop-problem-jobs problem)) 
      (setf list-aux (append list-aux (list (copy-job job)))))
    (make-job-shop-problem
     :name (job-shop-problem-name problem)
     :n.jobs (job-shop-problem-n.jobs problem)
     :n.machines (job-shop-problem-n.machines problem)
     :jobs list-aux)))
	

(defun copy-job (job)
  (let ((aux-tasks nil))
    (dolist (task (job-shop-job-tasks job))
      (setf aux-tasks (append aux-tasks (list (copy-task task)))))
    (make-job-shop-job
     :job.nr (job-shop-job-job.nr job)
     :tasks aux-tasks)))

(defun copy-task (task)
  (make-job-shop-task
   :job.nr (job-shop-task-job.nr task)
   :task.nr (job-shop-task-task.nr task)
   :machine.nr (job-shop-task-machine.nr task)
   :duration (job-shop-task-duration task)
   :start.time (job-shop-task-start.time task)))

		
