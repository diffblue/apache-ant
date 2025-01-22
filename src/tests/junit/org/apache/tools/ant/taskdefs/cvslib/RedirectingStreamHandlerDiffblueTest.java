package org.apache.tools.ant.taskdefs.cvslib;

import static org.junit.Assert.assertEquals;
import org.junit.Test;

public class RedirectingStreamHandlerDiffblueTest {
  /**
   * Test {@link RedirectingStreamHandler#RedirectingStreamHandler(ChangeLogParser)}.
   * <p>
   * Method under test: {@link RedirectingStreamHandler#RedirectingStreamHandler(ChangeLogParser)}
   */
  @Test
  public void testNewRedirectingStreamHandler() {
    // Arrange, Act and Assert
    assertEquals("", (new RedirectingStreamHandler(new ChangeLogParser())).getErrors());
  }

  /**
   * Test {@link RedirectingStreamHandler#getErrors()}.
   * <p>
   * Method under test: {@link RedirectingStreamHandler#getErrors()}
   */
  @Test
  public void testGetErrors() {
    // Arrange, Act and Assert
    assertEquals("", (new RedirectingStreamHandler(new ChangeLogParser())).getErrors());
  }
}
