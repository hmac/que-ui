const jobStatus = (job) => {
  if (job.destroyed) { return "destroyed"; }
  if (!job.retryable) { return "failed"; }
  if (job.scheduled_for_future) { return "scheduled"; }
  return "queued";
};

export default jobStatus;
