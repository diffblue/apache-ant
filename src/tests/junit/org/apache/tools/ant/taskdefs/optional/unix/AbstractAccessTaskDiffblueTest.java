package org.apache.tools.ant.taskdefs.optional.unix;

import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertThrows;
import static org.junit.Assert.assertTrue;
import org.apache.tools.ant.BuildException;
import org.apache.tools.ant.types.Commandline;
import org.junit.Test;

public class AbstractAccessTaskDiffblueTest {
  /**
   * Test {@link AbstractAccessTask#setCommand(Commandline)}.
   * <p>
   * Method under test: {@link AbstractAccessTask#setCommand(Commandline)}
   */
  @Test
  public void testSetCommand() {
    // Arrange
    Chgrp chgrp = new Chgrp();

    // Act and Assert
    assertThrows(BuildException.class, () -> chgrp.setCommand(new Commandline("To Process")));
  }

  /**
   * Test {@link AbstractAccessTask#setSkipEmptyFilesets(boolean)}.
   * <p>
   * Method under test: {@link AbstractAccessTask#setSkipEmptyFilesets(boolean)}
   */
  @Test
  public void testSetSkipEmptyFilesets() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chgrp()).setSkipEmptyFilesets(true));
  }

  /**
   * Test {@link AbstractAccessTask#setAddsourcefile(boolean)}.
   * <p>
   * Method under test: {@link AbstractAccessTask#setAddsourcefile(boolean)}
   */
  @Test
  public void testSetAddsourcefile() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new Chgrp()).setAddsourcefile(true));
  }

  /**
   * Test {@link AbstractAccessTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chgrp} (default constructor) OsFamily is {@code unix}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAccessTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChgrpOsFamilyIsUnix_thenReturnTrue() {
    // Arrange
    Chgrp chgrp = new Chgrp();
    chgrp.setOsFamily("unix");

    // Act and Assert
    assertTrue(chgrp.isValidOs());
  }

  /**
   * Test {@link AbstractAccessTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chgrp} (default constructor) OsFamily is {@code windows}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAccessTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChgrpOsFamilyIsWindows_thenReturnFalse() {
    // Arrange
    Chgrp chgrp = new Chgrp();
    chgrp.setOsFamily("windows");

    // Act and Assert
    assertFalse(chgrp.isValidOs());
  }

  /**
   * Test {@link AbstractAccessTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chgrp} (default constructor) Os is {@code unix}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAccessTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChgrpOsIsUnix_thenReturnFalse() {
    // Arrange
    Chgrp chgrp = new Chgrp();
    chgrp.setOs("unix");

    // Act and Assert
    assertFalse(chgrp.isValidOs());
  }

  /**
   * Test {@link AbstractAccessTask#isValidOs()}.
   * <ul>
   *   <li>Given {@link Chgrp} (default constructor).</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link AbstractAccessTask#isValidOs()}
   */
  @Test
  public void testIsValidOs_givenChgrp_thenReturnTrue() {
    // Arrange, Act and Assert
    assertTrue((new Chgrp()).isValidOs());
  }
}
