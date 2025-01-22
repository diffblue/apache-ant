package org.apache.tools.ant.taskdefs;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class HostInfoDiffblueTest {
  /**
   * Test new {@link HostInfo} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link HostInfo}
   */
  @Test
  public void testNewHostInfo() {
    // Arrange and Act
    HostInfo actualHostInfo = new HostInfo();

    // Assert
    Location location = actualHostInfo.getLocation();
    assertNull(location.getFileName());
    assertNull(actualHostInfo.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualHostInfo.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualHostInfo.getTaskName());
    assertNull(actualHostInfo.getTaskType());
    assertNull(actualHostInfo.getProject());
    assertNull(actualHostInfo.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualHostInfo, runtimeConfigurableWrapper.getProxy());
  }
}
