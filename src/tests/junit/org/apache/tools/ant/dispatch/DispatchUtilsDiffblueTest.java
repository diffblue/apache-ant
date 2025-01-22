package org.apache.tools.ant.dispatch;

import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.PickOneTask;
import org.junit.Test;

public class DispatchUtilsDiffblueTest {
  /**
   * Test {@link DispatchUtils#execute(Object)}.
   * <ul>
   *   <li>Given {@code action}.</li>
   *   <li>When {@link PickOneTask} (default constructor) Action is {@code action}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DispatchUtils#execute(Object)}
   */
  @Test
  public void testExecute_givenAction_whenPickOneTaskActionIsAction_thenThrowBuildException() throws BuildException {
    // Arrange
    PickOneTask pickOneTask = new PickOneTask();
    pickOneTask.setAction("action");

    // Act and Assert
    assertThrows(BuildException.class, () -> DispatchUtils.execute(pickOneTask));
  }

  /**
   * Test {@link DispatchUtils#execute(Object)}.
   * <ul>
   *   <li>Given empty string.</li>
   *   <li>When {@link PickOneTask} (default constructor) Action is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link DispatchUtils#execute(Object)}
   */
  @Test
  public void testExecute_givenEmptyString_whenPickOneTaskActionIsEmptyString() throws BuildException {
    // Arrange
    PickOneTask pickOneTask = new PickOneTask();
    pickOneTask.setAction("");

    // Act and Assert
    assertThrows(BuildException.class, () -> DispatchUtils.execute(pickOneTask));
  }

  /**
   * Test {@link DispatchUtils#execute(Object)}.
   * <ul>
   *   <li>When {@link PickOneTask} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DispatchUtils#execute(Object)}
   */
  @Test
  public void testExecute_whenPickOneTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> DispatchUtils.execute(new PickOneTask()));
  }

  /**
   * Test {@link DispatchUtils#execute(Object)}.
   * <ul>
   *   <li>When {@code Task}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link DispatchUtils#execute(Object)}
   */
  @Test
  public void testExecute_whenTask_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> DispatchUtils.execute("Task"));
  }
}
