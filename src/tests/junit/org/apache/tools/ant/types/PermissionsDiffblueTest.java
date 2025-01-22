package org.apache.tools.ant.types;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertFalse;
import static org.junit.Assert.assertNull;
import static org.junit.Assert.assertTrue;
import java.security.AllPermission;
import org.apache.tools.ant.Location;
import org.apache.tools.ant.types.Permissions.Permission;
import org.junit.Test;

public class PermissionsDiffblueTest {
  /**
   * Test {@link Permissions#Permissions()}.
   * <p>
   * Method under test: {@link Permissions#Permissions()}
   */
  @Test
  public void testNewPermissions() {
    // Arrange and Act
    Permissions actualPermissions = new Permissions();

    // Assert
    Location location = actualPermissions.getLocation();
    assertNull(location.getFileName());
    assertNull(actualPermissions.getDescription());
    assertNull(actualPermissions.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test {@link Permissions#Permissions(boolean)}.
   * <p>
   * Method under test: {@link Permissions#Permissions(boolean)}
   */
  @Test
  public void testNewPermissions2() {
    // Arrange and Act
    Permissions actualPermissions = new Permissions(true);

    // Assert
    Location location = actualPermissions.getLocation();
    assertNull(location.getFileName());
    assertNull(actualPermissions.getDescription());
    assertNull(actualPermissions.getProject());
    assertEquals(0, location.getColumnNumber());
    assertEquals(0, location.getLineNumber());
  }

  /**
   * Test Permission getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>default or parameterless constructor of {@link Permission}
   *   <li>{@link Permission#toString()}
   *   <li>{@link Permission#getActions()}
   *   <li>{@link Permission#getClassName()}
   *   <li>{@link Permission#getName()}
   * </ul>
   */
  @Test
  public void testPermissionGettersAndSetters() {
    // Arrange and Act
    Permission actualPermission = new Permission();
    String actualToStringResult = actualPermission.toString();
    String actualActions = actualPermission.getActions();
    String actualClassName = actualPermission.getClassName();

    // Assert
    assertEquals("Permission: null (\"null\", \"null\")", actualToStringResult);
    assertNull(actualActions);
    assertNull(actualClassName);
    assertNull(actualPermission.getName());
  }

  /**
   * Test Permission {@link Permission#matches(Permission)}.
   * <ul>
   *   <li>Given {@link Permission} (default constructor) Class is {@code A Class}.</li>
   *   <li>Then return {@code false}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Permission#matches(java.security.Permission)}
   */
  @Test
  public void testPermissionMatches_givenPermissionClassIsAClass_thenReturnFalse() {
    // Arrange
    Permission permission = new Permission();
    permission.setClass("A Class");

    // Act and Assert
    assertFalse(permission.matches(new AllPermission("foo", "foo")));
  }

  /**
   * Test Permission {@link Permission#matches(Permission)}.
   * <ul>
   *   <li>Given {@link Permission} (default constructor) Name is {@code <all permissions>}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Permission#matches(java.security.Permission)}
   */
  @Test
  public void testPermissionMatches_givenPermissionNameIsAllPermissions_thenReturnTrue() {
    // Arrange
    Permission permission = new Permission();
    permission.setName("<all permissions>");
    permission.setClass("java.security.AllPermission");

    // Act and Assert
    assertTrue(permission.matches(new AllPermission("foo", "foo")));
  }

  /**
   * Test Permission {@link Permission#matches(Permission)}.
   * <ul>
   *   <li>Given {@link Permission} (default constructor) Name is {@code *}.</li>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Permission#matches(java.security.Permission)}
   */
  @Test
  public void testPermissionMatches_givenPermissionNameIsAsterisk_thenReturnTrue() {
    // Arrange
    Permission permission = new Permission();
    permission.setName("*");
    permission.setClass("java.security.AllPermission");

    // Act and Assert
    assertTrue(permission.matches(new AllPermission("foo", "foo")));
  }

  /**
   * Test Permission {@link Permission#matches(Permission)}.
   * <ul>
   *   <li>Given {@link Permission} (default constructor) Name is {@code AllPermission}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Permission#matches(java.security.Permission)}
   */
  @Test
  public void testPermissionMatches_givenPermissionNameIsJavaSecurityAllPermission() {
    // Arrange
    Permission permission = new Permission();
    permission.setName("java.security.AllPermission");
    permission.setClass("java.security.AllPermission");

    // Act and Assert
    assertFalse(permission.matches(new AllPermission("foo", "foo")));
  }

  /**
   * Test Permission {@link Permission#matches(Permission)}.
   * <ul>
   *   <li>Given {@link Permission} (default constructor) Name is {@code AllPermission*}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Permission#matches(java.security.Permission)}
   */
  @Test
  public void testPermissionMatches_givenPermissionNameIsJavaSecurityAllPermission2() {
    // Arrange
    Permission permission = new Permission();
    permission.setName("java.security.AllPermission*");
    permission.setClass("java.security.AllPermission");

    // Act and Assert
    assertFalse(permission.matches(new AllPermission("foo", "foo")));
  }

  /**
   * Test Permission {@link Permission#matches(Permission)}.
   * <ul>
   *   <li>Then return {@code true}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Permission#matches(java.security.Permission)}
   */
  @Test
  public void testPermissionMatches_thenReturnTrue() {
    // Arrange
    Permission permission = new Permission();
    permission.setClass("java.security.AllPermission");

    // Act and Assert
    assertTrue(permission.matches(new AllPermission("foo", "foo")));
  }

  /**
   * Test Permission {@link Permission#setActions(String)}.
   * <ul>
   *   <li>Given {@link Permission} (default constructor).</li>
   *   <li>Then {@link Permission} (default constructor) Actions is {@code Actions}.</li>
   * </ul>
   * <p>
   * Method under test: {@link Permission#setActions(String)}
   */
  @Test
  public void testPermissionSetActions_givenPermission_thenPermissionActionsIsActions() {
    // Arrange
    Permission permission = new Permission();

    // Act
    permission.setActions("Actions");

    // Assert
    assertEquals("Actions", permission.getActions());
  }

  /**
   * Test Permission {@link Permission#setActions(String)}.
   * <ul>
   *   <li>Then {@link Permission} (default constructor) Actions is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link Permission#setActions(String)}
   */
  @Test
  public void testPermissionSetActions_thenPermissionActionsIsEmptyString() {
    // Arrange
    Permission permission = new Permission();
    permission.setClass("A Class");
    permission.setName("A Name");

    // Act
    permission.setActions("");

    // Assert
    assertEquals("", permission.getActions());
  }

  /**
   * Test Permission {@link Permission#setClass(String)}.
   * <p>
   * Method under test: {@link Permission#setClass(String)}
   */
  @Test
  public void testPermissionSetClass() {
    // Arrange
    Permission permission = new Permission();

    // Act
    permission.setClass("A Class");

    // Assert
    assertEquals("A Class", permission.getClassName());
  }

  /**
   * Test Permission {@link Permission#setName(String)}.
   * <p>
   * Method under test: {@link Permission#setName(String)}
   */
  @Test
  public void testPermissionSetName() {
    // Arrange
    Permission permission = new Permission();

    // Act
    permission.setName("A Name");

    // Assert
    assertEquals("A Name", permission.getName());
  }
}
