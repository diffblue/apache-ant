package org.apache.tools.ant.types.selectors;

import static org.junit.Assert.assertThrows;
import java.io.File;
import java.nio.file.Paths;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class PosixPermissionsSelectorDiffblueTest {
  /**
   * Test {@link PosixPermissionsSelector#setPermissions(String)}.
   * <p>
   * Method under test: {@link PosixPermissionsSelector#setPermissions(String)}
   */
  @Test
  public void testSetPermissions() {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new PosixPermissionsSelector()).setPermissions("Permissions"));
  }

  /**
   * Test {@link PosixPermissionsSelector#isSelected(File, String, File)} with {@code basedir}, {@code filename}, {@code file}.
   * <p>
   * Method under test: {@link PosixPermissionsSelector#isSelected(File, String, File)}
   */
  @Test
  public void testIsSelectedWithBasedirFilenameFile() {
    // Arrange
    PosixPermissionsSelector posixPermissionsSelector = new PosixPermissionsSelector();
    File basedir = Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile();

    // Act and Assert
    assertThrows(BuildException.class, () -> posixPermissionsSelector.isSelected(basedir, "foo.txt",
        Paths.get(System.getProperty("java.io.tmpdir"), "test.txt").toFile()));
  }
}
