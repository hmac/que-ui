import React, { Component } from "react";
import { connect } from "react-redux";

class LastUpdated_ extends Component {
  constructor(props) {
    super(props);
    this.state = { now: Date.now() };
  }

  render() {
    return (
      <span className={ this.props.className }>
        { this.content(this.props.last_updated) }
      </span>
    );
  }

  content(last_updated) {
    if (last_updated === null) { return "No connection"; }
    const delta = Math.round((Date.now() - last_updated)/1000);
    if (delta < 2) { return "Connected"; }
    return `Last updated: ${ delta } seconds ago`;
  }

  componentWillMount() {
    this.timer = setInterval(
      () => { this.setState({ now: Date.now() }); },
      1000
    );
  }

  componentDidMountUnmount() {
    clearInterval(this.timer);
  }
}

const LastUpdated = connect(
  ({ last_updated }) => { return { last_updated }; }
)(LastUpdated_);

export default LastUpdated;
