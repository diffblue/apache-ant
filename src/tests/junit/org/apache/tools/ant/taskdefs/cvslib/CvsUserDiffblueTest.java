package org.apache.tools.ant.taskdefs.cvslib;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertThrows;
import org.apache.tools.ant.BuildException;
import org.junit.Test;

public class CvsUserDiffblueTest {
  /**
   * Test {@link CvsUser#validate()}.
   * <ul>
   *   <li>Given {@link CvsUser} (default constructor) Displayname is {@code null}.</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsUser#validate()}
   */
  @Test
  public void testValidate_givenCvsUserDisplaynameIsNull_thenThrowBuildException() throws BuildException {
    // Arrange
    CvsUser cvsUser = new CvsUser();
    cvsUser.setUserid("foo");
    cvsUser.setDisplayname(null);

    // Act and Assert
    assertThrows(BuildException.class, () -> cvsUser.validate());
  }

  /**
   * Test {@link CvsUser#validate()}.
   * <ul>
   *   <li>Given {@link CvsUser} (default constructor).</li>
   *   <li>Then throw {@link BuildException}.</li>
   * </ul>
   * <p>
   * Method under test: {@link CvsUser#validate()}
   */
  @Test
  public void testValidate_givenCvsUser_thenThrowBuildException() throws BuildException {
    // Arrange, Act and Assert
    assertThrows(BuildException.class, () -> (new CvsUser()).validate());
  }

  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link CvsUser}
   *   <li>{@link CvsUser#setDisplayname(String)}
   *   <li>{@link CvsUser#setUserid(String)}
   *   <li>{@link CvsUser#getDisplayname()}
   *   <li>{@link CvsUser#getUserID()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    CvsUser actualCvsUser = new CvsUser();
    actualCvsUser.setDisplayname("Display Name");
    actualCvsUser.setUserid("User ID");
    String actualDisplayname = actualCvsUser.getDisplayname();

    // Assert
    assertEquals("Display Name", actualDisplayname);
    assertEquals("User ID", actualCvsUser.getUserID());
  }
}
