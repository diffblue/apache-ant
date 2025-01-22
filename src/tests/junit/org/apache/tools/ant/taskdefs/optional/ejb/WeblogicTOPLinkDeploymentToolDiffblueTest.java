package org.apache.tools.ant.taskdefs.optional.ejb;

import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertThrows;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class WeblogicTOPLinkDeploymentToolDiffblueTest {
  /**
   * Test {@link WeblogicTOPLinkDeploymentTool#validateConfigured()}.
   * <ul>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link WeblogicTOPLinkDeploymentTool#validateConfigured()}
   */
  @Test
  public void testValidateConfigured_thenThrowBuildException() throws BuildException {
    // Arrange
    WeblogicTOPLinkDeploymentTool weblogicTOPLinkDeploymentTool = new WeblogicTOPLinkDeploymentTool();
    weblogicTOPLinkDeploymentTool.setDestdir(Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile());

    // Act and Assert
    assertThrows(BuildException.class, () -> weblogicTOPLinkDeploymentTool.validateConfigured());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link WeblogicTOPLinkDeploymentTool}
   *   <li>{@link WeblogicTOPLinkDeploymentTool#setToplinkdescriptor(String)}
   *   <li>{@link WeblogicTOPLinkDeploymentTool#setToplinkdtd(String)}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    WeblogicTOPLinkDeploymentTool actualWeblogicTOPLinkDeploymentTool = new WeblogicTOPLinkDeploymentTool();
    actualWeblogicTOPLinkDeploymentTool.setToplinkdescriptor("In String");
    actualWeblogicTOPLinkDeploymentTool.setToplinkdtd("In String");

    // Assert
    assertNull(actualWeblogicTOPLinkDeploymentTool.getDestDir());
    assertNull(actualWeblogicTOPLinkDeploymentTool.getJvmDebugLevel());
    assertNull(actualWeblogicTOPLinkDeploymentTool.getEjbcClass());
    assertNull(actualWeblogicTOPLinkDeploymentTool.getTask());
    assertNull(actualWeblogicTOPLinkDeploymentTool.getConfig());
  }
}
