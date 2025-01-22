package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import java.io.File;
import org.junit.Test;

public class OrionDeploymentToolDiffblueTest {
  /**
   * Test {@link OrionDeploymentTool#getVendorOutputJarFile(String)}.
   * <p>
   * Method under test: {@link OrionDeploymentTool#getVendorOutputJarFile(String)}
   */
  @Test
  public void testGetVendorOutputJarFile() {
    // Arrange and Act
    File actualVendorOutputJarFile = (new OrionDeploymentTool()).getVendorOutputJarFile("Base Name");

    // Assert
    assertEquals("Base Name.jar", actualVendorOutputJarFile.getName());
    assertFalse(actualVendorOutputJarFile.isAbsolute());
  }

  /**
   * Test new {@link OrionDeploymentTool} (default constructor).
   * <p>
   * Method under test: default or parameterless constructor of {@link OrionDeploymentTool}
   */
  @Test
  public void testNewOrionDeploymentTool() {
    // Arrange and Act
    OrionDeploymentTool actualOrionDeploymentTool = new OrionDeploymentTool();

    // Assert
    assertNull(actualOrionDeploymentTool.getDestDir());
    assertNull(actualOrionDeploymentTool.getTask());
    assertNull(actualOrionDeploymentTool.getConfig());
  }
}
