package org.apache.tools.ant.types.resources;

import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import java.io.IOException;
import java.io.OutputStream;
import org.apache.tools.ant.taskdefs.LogOutputStream;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class LogOutputResourceDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link LogOutputResource#getAppendOutputStream()}
   *   <li>{@link LogOutputResource#getOutputStream()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() throws IOException {
    // Arrange
    LogOutputResource logOutputResource = new LogOutputResource(Path.systemBootClasspath, 1);

    // Act
    OutputStream actualAppendOutputStream = logOutputResource.getAppendOutputStream();

    // Assert
    assertTrue(actualAppendOutputStream instanceof LogOutputStream);
    assertSame(actualAppendOutputStream, logOutputResource.getOutputStream());
  }
}
