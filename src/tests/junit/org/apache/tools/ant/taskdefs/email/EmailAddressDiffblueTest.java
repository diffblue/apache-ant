package org.apache.tools.ant.taskdefs.email;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;
import org.junit.Test;

public class EmailAddressDiffblueTest {
  /**
   * Test getters and setters.
   * <p>
   * Methods under test:
   * <ul>
   *   <li>{@link EmailAddress#EmailAddress()}
   *   <li>{@link EmailAddress#setAddress(String)}
   *   <li>{@link EmailAddress#setName(String)}
   *   <li>{@link EmailAddress#getAddress()}
   *   <li>{@link EmailAddress#getName()}
   * </ul>
   */
  @Test
  public void testGettersAndSetters() {
    // Arrange and Act
    EmailAddress actualEmailAddress = new EmailAddress();
    actualEmailAddress.setAddress("42 Main St");
    actualEmailAddress.setName("Name");
    String actualAddress = actualEmailAddress.getAddress();

    // Assert
    assertEquals("42 Main St", actualAddress);
    assertEquals("Name", actualEmailAddress.getName());
  }

  /**
   * Test {@link EmailAddress#EmailAddress(String)}.
   * <ul>
   *   <li>When {@code Email}.</li>
   *   <li>Then return Address is {@code Email}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailAddress#EmailAddress(String)}
   */
  @Test
  public void testNewEmailAddress_whenEmail_thenReturnAddressIsEmail() {
    // Arrange and Act
    EmailAddress actualEmailAddress = new EmailAddress("Email");

    // Assert
    assertEquals("Email", actualEmailAddress.getAddress());
    assertNull(actualEmailAddress.getName());
  }

  /**
   * Test {@link EmailAddress#EmailAddress(String)}.
   * <ul>
   *   <li>When empty string.</li>
   *   <li>Then return Address is empty string.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailAddress#EmailAddress(String)}
   */
  @Test
  public void testNewEmailAddress_whenEmptyString_thenReturnAddressIsEmptyString() {
    // Arrange and Act
    EmailAddress actualEmailAddress = new EmailAddress("");

    // Assert
    assertEquals("", actualEmailAddress.getAddress());
    assertEquals("", actualEmailAddress.getName());
  }

  /**
   * Test {@link EmailAddress#EmailAddress(String)}.
   * <ul>
   *   <li>When {@code jane.doe@example.org}.</li>
   *   <li>Then return Address is {@code jane.doe@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailAddress#EmailAddress(String)}
   */
  @Test
  public void testNewEmailAddress_whenJaneDoeExampleOrg_thenReturnAddressIsJaneDoeExampleOrg() {
    // Arrange and Act
    EmailAddress actualEmailAddress = new EmailAddress("jane.doe@example.org");

    // Assert
    assertEquals("jane.doe@example.org", actualEmailAddress.getAddress());
    assertNull(actualEmailAddress.getName());
  }

  /**
   * Test {@link EmailAddress#toString()}.
   * <ul>
   *   <li>Then return {@code foo <jane.doe@example.org>}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailAddress#toString()}
   */
  @Test
  public void testToString_thenReturnFooJaneDoeExampleOrg() {
    // Arrange
    EmailAddress emailAddress = new EmailAddress("jane.doe@example.org");
    emailAddress.setName("foo");

    // Act and Assert
    assertEquals("foo <jane.doe@example.org>", emailAddress.toString());
  }

  /**
   * Test {@link EmailAddress#toString()}.
   * <ul>
   *   <li>Then return {@code jane.doe@example.org}.</li>
   * </ul>
   * <p>
   * Method under test: {@link EmailAddress#toString()}
   */
  @Test
  public void testToString_thenReturnJaneDoeExampleOrg() {
    // Arrange, Act and Assert
    assertEquals("jane.doe@example.org", (new EmailAddress("jane.doe@example.org")).toString());
  }
}
