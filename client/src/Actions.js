const RECEIVE_SUMMARIES = "RECEIVE_SUMMARIES";
const RECEIVE_JOBS = "RECEIVE_JOBS";
const RECEIVE_FAILURES = "RECEIVE_FAILURES";
const RECEIVE_JOB = "RECEIVE_JOB";
const RECEIVE_WORKERS = "RECEIVE_WORKERS";

const URL = "http://localhost:8080";

const fetchSummaries = (dispatch) => {
  return fetch(`${ URL }/queue-summary`).then(
    (response) => response.json(),
  ).then(
    (json) => { dispatch({type: RECEIVE_SUMMARIES, summaries: json}) },
    (error) => console.log("error", error),
  )
};

const fetchJobs = (search) => {
  return (dispatch) => {
    return fetch(`${ URL }/jobs${ search }`).then(
      (response) => response.json(),
    ).then(
      (json) => { dispatch({type: RECEIVE_JOBS, jobs: json}) },
      (error) => console.log("error", error),
    )
  };
};

const fetchJob = (id) => {
  return (dispatch) => {
    return fetch(`${ URL }/jobs/${ id }`).then(
      (response) => response.json(),
    ).then(
      (json) => { dispatch({type: RECEIVE_JOB, job: json}) },
      (error) => console.log("error", error),
    )
  };
};

const fetchFailedJobs = (dispatch) => {
  return fetch(`${ URL }/failures`).then(
    (response) => response.json(),
  ).then(
    (json) => { dispatch({type: RECEIVE_FAILURES, failures: json}) },
    (error) => console.log("error", error),
  )
};

const fetchWorkers = (dispatch) => {
  return fetch(`${ URL }/workers`).then(
    (response) => response.json(),
  ).then(
    (json) => { dispatch({type: RECEIVE_WORKERS, workers: json}) },
    (error) => console.log("error", error),
  )
};

const retryJob = (id) => {
  return (dispatch) => {
    return fetch(`${ URL }/jobs/${ id }/retry`, { method: "POST" }).then(
      (response) => response.json(),
    ).then(
      (json) => { dispatch({type: RECEIVE_JOB, job: json}) },
      (error) => console.log("error", error),
    )
  };
};

const destroyJob = (id) => {
  return (dispatch) => {
    return fetch(`${ URL }/jobs/${ id }/destroy`, { method: "POST" }).then(
      (response) => response.json(),
    ).then(
      (json) => { dispatch({type: RECEIVE_JOB, job: null}) },
      (error) => console.log("error", error),
    )
  };
};

const retryJobs = (job_class) => {
  return (dispatch) => {
    return fetch(`${ URL }/failures/${ job_class }/retry`, { method: "POST" }).then(
      (response) => response.json(),
      (error) => console.log("error", error),
    ).then(
      (json) => fetchFailedJobs(dispatch),
    )
  };
};

const destroyJobs = (job_class) => {
  return (dispatch) => {
    return fetch(`${ URL }/failures/${ job_class }/destroy`, { method: "POST" }).then(
      (response) => response.json(),
      (error) => console.log("error", error),
    ).then(
      (json) => { fetchFailedJobs(dispatch) },
    )
  };
};

export {
  fetchSummaries,
  fetchJobs,
  fetchFailedJobs,
  fetchJob,
  fetchWorkers,
  retryJob,
  destroyJob,
  retryJobs,
  destroyJobs,
  RECEIVE_SUMMARIES,
  RECEIVE_JOBS,
  RECEIVE_FAILURES,
  RECEIVE_JOB,
  RECEIVE_WORKERS
};
