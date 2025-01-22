package org.apache.tools.ant.taskdefs.optional.net;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertSame;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.RuntimeConfigurable;
import org.junit.Test;

public class SetProxyDiffblueTest {
  /**
   * Test new {@link SetProxy} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link SetProxy}
   */
  @Test
  public void testNewSetProxy() {
    // Arrange and Act
    SetProxy actualSetProxy = new SetProxy();

    // Assert
    Location location = actualSetProxy.getLocation();
    assertNull(location.getFileName());
    assertNull(actualSetProxy.getDescription());
    RuntimeConfigurable runtimeConfigurableWrapper = actualSetProxy.getRuntimeConfigurableWrapper();
    assertNull(runtimeConfigurableWrapper.getElementTag());
    assertNull(runtimeConfigurableWrapper.getId());
    assertNull(runtimeConfigurableWrapper.getPolyType());
    assertNull(actualSetProxy.getTaskName());
    assertNull(actualSetProxy.getTaskType());
    assertNull(actualSetProxy.proxyHost);
    assertNull(actualSetProxy.getProject());
    assertNull(actualSetProxy.getOwningTarget());
    assertNull(runtimeConfigurableWrapper.getAttributes());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
    assertEquals(80, actualSetProxy.proxyPort);
    assertTrue(runtimeConfigurableWrapper.getAttributeMap().isEmpty());
    assertSame(actualSetProxy, runtimeConfigurableWrapper.getProxy());
  }
}
