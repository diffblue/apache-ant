package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class CloseResourcesDiffblueTest {
  /**
   * Test new {@link CloseResources} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link CloseResources}
   */
  @Test
  public void testNewCloseResources() {
    // Arrange and Act
    CloseResources actualCloseResources = new CloseResources();

    // Assert
    Location location = actualCloseResources.getLocation();
    assertNull(location.getFileName());
    assertNull(actualCloseResources.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualCloseResources.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualCloseResources.getTaskName());
    assertNull(actualCloseResources.getTaskType());
    assertNull(actualCloseResources.getProject());
    assertNull(actualCloseResources.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualCloseResources, runtimeConfigurableWrapper.getProxy());
  }
}
