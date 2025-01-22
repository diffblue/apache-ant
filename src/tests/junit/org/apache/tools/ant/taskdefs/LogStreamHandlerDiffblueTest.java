package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertTrue;
import java.io.OutputStream;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.junit.Test;

public class LogStreamHandlerDiffblueTest {
  /**
   * Test {@link LogStreamHandler#LogStreamHandler(Task, int, int)}.
   * <p>
   * Method under test: {@link LogStreamHandler#LogStreamHandler(Task, int, int)}
   */
  @Test
  public void testNewLogStreamHandler() {
    // Arrange and Act
    LogStreamHandler actualLogStreamHandler = new LogStreamHandler(new TaskAdapter(), 1, 1);

    // Assert
    OutputStream err = actualLogStreamHandler.getErr();
    assertTrue(err instanceof LogOutputStream);
    OutputStream out = actualLogStreamHandler.getOut();
    assertTrue(out instanceof LogOutputStream);
    assertEquals(1, ((LogOutputStream) err).getMessageLevel());
    assertEquals(1, ((LogOutputStream) out).getMessageLevel());
  }
}
