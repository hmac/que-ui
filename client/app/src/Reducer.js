import { RECEIVE_SUMMARIES, RECEIVE_JOBS, RECEIVE_FAILURES, RECEIVE_JOB } from "./Actions";

const JOB_ROUTE = "JOB_ROUTE";
const JOBS_ROUTE = "JOBS_ROUTE";
const QUEUE_SUMMARY_ROUTE = "QUEUE_SUMMARY_ROUTE";
const WORKERS_ROUTE = "WORKERS_ROUTE";
const FAILURES_ROUTE = "FAILURES_ROUTE";

const initialState = {
  route: QUEUE_SUMMARY_ROUTE,
  summaries: [],
  jobs: [],
  failures: [],
};

const reducer = (state, action) => {
  switch(action.type) {
  case RECEIVE_SUMMARIES:
    return { ...state, route: QUEUE_SUMMARY_ROUTE, summaries: action.summaries };
  case RECEIVE_JOBS:
    return { ...state, route: JOBS_ROUTE, jobs: action.jobs };
  case RECEIVE_FAILURES:
    return { ...state, route: FAILURES_ROUTE, failures: action.failures };
  case RECEIVE_JOB:
    return {...state, route: JOB_ROUTE, job: action.job };
  case "@@router/LOCATION_CHANGE":
    return state;
  default:
    return initialState;
  }
};

export default reducer;
