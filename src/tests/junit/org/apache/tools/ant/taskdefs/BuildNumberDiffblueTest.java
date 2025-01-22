package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class BuildNumberDiffblueTest {
  /**
   * Test new {@link BuildNumber} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link BuildNumber}
   */
  @Test
  public void testNewBuildNumber() {
    // Arrange and Act
    BuildNumber actualBuildNumber = new BuildNumber();

    // Assert
    Location location = actualBuildNumber.getLocation();
    assertNull(location.getFileName());
    assertNull(actualBuildNumber.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualBuildNumber.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualBuildNumber.getTaskName());
    assertNull(actualBuildNumber.getTaskType());
    assertNull(actualBuildNumber.getProject());
    assertNull(actualBuildNumber.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualBuildNumber, runtimeConfigurableWrapper.getProxy());
  }
}
