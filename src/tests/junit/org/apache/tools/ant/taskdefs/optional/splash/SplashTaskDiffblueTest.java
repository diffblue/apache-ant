package org.apache.tools.ant.taskdefs.optional.splash;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class SplashTaskDiffblueTest {
  /**
   * Test new {@link SplashTask} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SplashTask}
   */
  @Test
  public void testNewSplashTask() {
    // Arrange and Act
    SplashTask actualSplashTask = new SplashTask();

    // Assert
    Location location = actualSplashTask.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSplashTask.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSplashTask.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSplashTask.getTaskName());
    assertNull(actualSplashTask.getTaskType());
    assertNull(actualSplashTask.getProject());
    assertNull(actualSplashTask.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSplashTask, runtimeConfigurableWrapper.getProxy());
  }
}
