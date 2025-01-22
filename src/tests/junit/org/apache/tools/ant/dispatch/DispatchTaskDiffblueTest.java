package org.apache.tools.ant.dispatch;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.apache.tools.ant.PickOneTask;
import org.apache.tools.ant.taskdefs.optional.unix.Symlink;
import org.junit.Test;

public class DispatchTaskDiffblueTest {
  /**
   * Test {@link DispatchTask#getActionParameterName()}.
   * <p>
   * Method under test: {@link DispatchTask#getActionParameterName()}
   */
  @Test
  public void testGetActionParameterName() {
    // Arrange, Act and Assert
    assertEquals("action", (new Symlink()).getActionParameterName());
  }

  /**
   * Test {@link DispatchTask#setAction(String)}.
   * <p>
   * Method under test: {@link DispatchTask#setAction(String)}
   */
  @Test
  public void testSetAction() {
    // Arrange
    PickOneTask pickOneTask = new PickOneTask();

    // Act
    pickOneTask.setAction("Action");

    // Assert
    assertEquals("Action", pickOneTask.getAction());
  }

  /**
   * Test {@link DispatchTask#getAction()}.
   * <p>
   * Method under test: {@link DispatchTask#getAction()}
   */
  @Test
  public void testGetAction() {
    // Arrange, Act and Assert
    assertNull((new Symlink()).getAction());
  }
}
