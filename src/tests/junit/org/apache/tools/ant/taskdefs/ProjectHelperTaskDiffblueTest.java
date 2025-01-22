package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class ProjectHelperTaskDiffblueTest {
  /**
   * Test new {@link ProjectHelperTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link ProjectHelperTask}
   */
  @Test
  public void testNewProjectHelperTask() {
    // Arrange and Act
    ProjectHelperTask actualProjectHelperTask = new ProjectHelperTask();

    // Assert
    Location location = actualProjectHelperTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualProjectHelperTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualProjectHelperTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualProjectHelperTask.getTaskName());
    assertNull(actualProjectHelperTask.getTaskType());
    assertNull(actualProjectHelperTask.getProject());
    assertNull(actualProjectHelperTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualProjectHelperTask, runtimeConfigurableWrapper.getProxy());
  }
}
