const RECEIVE_SUMMARIES = "RECEIVE_SUMMARIES";
const RECEIVE_JOBS = "RECEIVE_JOBS";
const RECEIVE_FAILURES = "RECEIVE_FAILURES";
const RECEIVE_JOB = "RECEIVE_JOB";

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

export {
  fetchSummaries,
  fetchJobs,
  fetchFailedJobs,
  fetchJob,
  RECEIVE_SUMMARIES,
  RECEIVE_JOBS,
  RECEIVE_FAILURES,
  RECEIVE_JOB
};
