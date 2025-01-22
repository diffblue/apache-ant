package org.apache.tools.ant.util;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.AntClassLoader;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.Project;
import org.apache.tools.ant.Target;
import org.junit.Test;

public class SecurityManagerUtilDiffblueTest {
  /**
   * Test {@link SecurityManagerUtil#isSetSecurityManagerAllowed()}.
   * <p>
   * Method under test: {@link SecurityManagerUtil#isSetSecurityManagerAllowed()}
   */
  @Test
  public void testIsSetSecurityManagerAllowed() {
    // Arrange, Act and Assert
    assertTrue(SecurityManagerUtil.isSetSecurityManagerAllowed());
  }

  /**
   * Test {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}.
   * <ul>
   *   <li>Given {@link AntClassLoader#AntClassLoader()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}
   */
  @Test
  public void testWarnOnSecurityManagerUsage_givenAntClassLoader() {
    // Arrange
    Project project = new Project();
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertFalse(SecurityManagerUtil.warnOnSecurityManagerUsage(project));
  }

  /**
   * Test {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}.
   * <ul>
   *   <li>Given {@code Object}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}
   */
  @Test
  public void testWarnOnSecurityManagerUsage_givenJavaLangObject() {
    // Arrange
    Project project = new Project();
    Class<Object> typeClass = Object.class;
    project.addDataTypeDefinition("ant.PropertyHelper", typeClass);
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertFalse(SecurityManagerUtil.warnOnSecurityManagerUsage(project));
  }

  /**
   * Test {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}.
   * <ul>
   *   <li>Given {@link Target#Target()}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}
   */
  @Test
  public void testWarnOnSecurityManagerUsage_givenTarget() throws BuildException {
    // Arrange
    Project project = new Project();
    project.addTarget("ant.PropertyHelper", new Target());
    project.addBuildListener(new AntClassLoader());

    // Act and Assert
    assertFalse(SecurityManagerUtil.warnOnSecurityManagerUsage(project));
  }

  /**
   * Test {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}.
   * <ul>
   *   <li>When {@code null}.</li>
   * </ul>
   * <p>
   * Method under test: {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}
   */
  @Test
  public void testWarnOnSecurityManagerUsage_whenNull() {
    // Arrange, Act and Assert
    assertFalse(SecurityManagerUtil.warnOnSecurityManagerUsage(null));
  }

  /**
   * Test {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}.
   * <ul>
   *   <li>When {@link Project} (default constructor).</li>
   * </ul>
   * <p>
   * Method under test: {@link SecurityManagerUtil#warnOnSecurityManagerUsage(Project)}
   */
  @Test
  public void testWarnOnSecurityManagerUsage_whenProject() {
    // Arrange, Act and Assert
    assertFalse(SecurityManagerUtil.warnOnSecurityManagerUsage(new Project()));
  }
}
