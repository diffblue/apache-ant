package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import org.apache.tools.ant.ProjectComponent;
import org.apache.tools.ant.Task;
import org.apache.tools.ant.TaskAdapter;
import org.apache.tools.ant.types.Path;
import org.junit.Test;

public class LogOutputStreamDiffblueTest {
  /**
   * Test {@link LogOutputStream#LogOutputStream(ProjectComponent)}.
   * <p>
   * Method under test: {@link LogOutputStream#LogOutputStream(ProjectComponent)}
   */
  @Test
  public void testNewLogOutputStream() {
    // Arrange, Act and Assert
    assertEquals(2, (new LogOutputStream(Path.systemBootClasspath)).getMessageLevel());
  }

  /**
   * Test {@link LogOutputStream#LogOutputStream(ProjectComponent, int)}.
   * <p>
   * Method under test: {@link LogOutputStream#LogOutputStream(ProjectComponent, int)}
   */
  @Test
  public void testNewLogOutputStream2() {
    // Arrange, Act and Assert
    assertEquals(1, (new LogOutputStream(Path.systemBootClasspath, 1)).getMessageLevel());
  }

  /**
   * Test {@link LogOutputStream#LogOutputStream(Task, int)}.
   * <p>
   * Method under test: {@link LogOutputStream#LogOutputStream(Task, int)}
   */
  @Test
  public void testNewLogOutputStream3() {
    // Arrange, Act and Assert
    assertEquals(1, (new LogOutputStream(new TaskAdapter(), 1)).getMessageLevel());
  }

  /**
   * Test {@link LogOutputStream#getMessageLevel()}.
   * <p>
   * Method under test: {@link LogOutputStream#getMessageLevel()}
   */
  @Test
  public void testGetMessageLevel() {
    // Arrange, Act and Assert
    assertEquals(2, (new LogOutputStream(Path.systemBootClasspath)).getMessageLevel());
  }
}
