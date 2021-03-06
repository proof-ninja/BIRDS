import React, { useEffect } from 'react';
import { connect } from 'unistore/react';
import { actions } from './stores/unistoreStore';
import FullscreenMessage from './common/FullscreenMessage.js';

function NotFound({ currentUser }) {
  useEffect(() => {
    document.title = 'BIRDS Editor - Not Found';
  }, []);

  // TODO FIXME XXX -
  // Now that the nav bar is gone do we just render link to new query?
  // Or just show modal saying "query not found" ok/close goes to new query?
  // with the query list open?
  if (currentUser) {
    return <FullscreenMessage>Not Found</FullscreenMessage>;
  }
  return <FullscreenMessage>Not Found</FullscreenMessage>;
}

export default connect(
  ['currentUser'],
  actions
)(NotFound);
