package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class SequentialDiffblueTest {
  /**
   * Test new {@link Sequential} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link Sequential}
   */
  @Test
  public void testNewSequential() {
    // Arrange and Act
    Sequential actualSequential = new Sequential();

    // Assert
    Location location = actualSequential.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSequential.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSequential.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSequential.getTaskName());
    assertNull(actualSequential.getTaskType());
    assertNull(actualSequential.getProject());
    assertNull(actualSequential.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSequential, runtimeConfigurableWrapper.getProxy());
  }
}
