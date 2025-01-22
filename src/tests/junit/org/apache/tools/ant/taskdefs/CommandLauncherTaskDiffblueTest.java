package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.apache.tools.ant.taskdefs.launcher.CommandLauncher;
import org.junit.Test;

public class CommandLauncherTaskDiffblueTest {
  /**
   * Test {@link CommandLauncherTask#addConfigured(CommandLauncher)}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CommandLauncherTask#addConfigured(CommandLauncher)}
   */
  @Test
  public void testAddConfigured_thenThrowBuildException() {
    // Arrange
    CommandLauncherTask commandLauncherTask = new CommandLauncherTask();
    commandLauncherTask.addConfigured(new CommandLauncher());

    // Act and Assert
    assertThrows(BuildException.class, () -> commandLauncherTask.addConfigured(new CommandLauncher()));
  }

  /**
   * Test new {@link CommandLauncherTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CommandLauncherTask}
   */
  @Test
  public void testNewCommandLauncherTask() {
    // Arrange and Act
    CommandLauncherTask actualCommandLauncherTask = new CommandLauncherTask();

    // Assert
    Location location = actualCommandLauncherTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCommandLauncherTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualCommandLauncherTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualCommandLauncherTask.getTaskName());
    assertNull(actualCommandLauncherTask.getTaskType());
    assertNull(actualCommandLauncherTask.getProject());
    assertNull(actualCommandLauncherTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualCommandLauncherTask, runtimeConfigurableWrapper.getProxy());
  }
}
